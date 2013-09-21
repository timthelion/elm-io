{- Public domain - Creative Commons ZERO
   http://creativecommons.org/publicdomain/zero/1.0/
   Timothy Hobbs                         -}

{- TODO centiseconds type, rename centiseconds to now in most places -}
module Animation where



{- Animations are values which change over time -}


data Animation a =
 Animation
  {numFrames: Int
  ,framerate: Int
  ,render: Int -> a {- frame -> a -}
  }

data ConcurrentAnimation a =
 ConcurrentAnimation
  {start: Int
  ,previousFrame: Int
  ,previousRender: Maybe a
  ,animation: Animation a}










{- This really isn't a good way to do things, but all the reasonable methods that occured to me require signal loops. I'd really like to use a signal loop to turn animation streams on and off, but I guess I'll just have to use Modes to ignore animation streams instead.-}
data Mode =
   AnyMode
 | Mode Int

data Command a =
   PlayAnimation {- adds an animation to be played to the que -}
    {mode: Mode
    ,animation: Animation a}
 | ClearCommandQue
    {mode: Mode}
 | ClearCommandQueAnd
    {mode: Mode
    ,command: Command a}
 | SetMode {- adds a SetMode command to the que -}
    {mode: Mode
    ,newMode: Mode}
 | AddConcurrentAnimation
    {mode: Mode
    ,animation: Animation a}
 | AddConcurrentAnimationImediately {- Does NOT clear the command que -}
    {mode: Mode
    ,animation: Animation a}
  



data AnimationPlayerState a =
 AnimationPlayerState
  {currentMode: Mode
  ,commandQue: [Command a]
  ,startOfCurrentAnimation: Int
  ,previousFrame: Int
  ,previousRender: Maybe a
  ,conncurrentAnimations: [ConcurrentAnimation a]
  ,renderedFrames: [a]}



{- An animation Player takes a stream of que commands and reacts to them. -}
animationPlayer: Signal (AnimationQueCommand a) -> Signal [a]
animationPlayer animationQueCommandS =
 let
  initialAnimationPlayerState =
   AnimationPlayerState
    {currentMode=0
    ,commandQue=[]
    ,startOfCurrentAnimation=0
    ,previousFrame=0-1
    ,previousRender=Nothing
    ,concurrentAnimations=[]
    ,renderedFrames=[]}

  processTickOrQueCommand (centiseconds,NoEvent qc) aps =
   if qc.mode == aps.currentMode || qc.mode == AnyMode || aps.currentMode == AnyMode
   then
    case qc of
     ClearCommandQue -> {aps|commandQue <- []}
     ClearCommandQueAnd -> {aps|commandQue <- [qc.command]}
     AddConcurrentAnimationImediately -> addConcurrentAnimation aps centiseconds qc.animation
     _ -> {aps|commandQue <- aps.commandQue ++ [qc]}
   else 
    aps
  processTickOrQueCommand (centiseconds,BEvent _) aps
   =  {aps|renderedFrames<-[]}
   |> processTick

  processTick: Int -> AnimationPlayerState a -> AnimationPlayerState a
  processTick centiseconds aps
   = aps
   |> processNonAnimationCommands centiseconds
   |> renderCurrentAnimation centiseconds
   |> renderConcurrentAnimations centiseconds



  addConcurrentAnimation aps centiseconds animation =
    {aps|concurrentAnimations<-
          ConcurrentAnimation
           {start=centiseconds
           ,previousFrame=0-1
           ,previousRender=Nothing
           ,animation=animation} :: aps.concurrentAnimations}

  processNonAnimationCommands: Int -> AnimationPlayerState a -> AnimationPlayerState a
  processNonAnimationCommands centiseconds aps
   =  (case aps.commandQue of
       (SetMode{newMode}::cs) -> {aps|currentMode<-newMode,commandQue<-cs} 
       (AddConcurrentAnimation{animation}::cs)->
         addConcurrentAnimation aps centiseconds animation)
   |> processNonAnimationCommands aps

  renderCurrentAnimation: Int -> AnimationPlayerState a -> AnimationPlayerState a
  renderCurrentAnimation centiseconds aps =
   case aps.commandQue of
    (PlayAnimation{animation}::cs) ->
      let
       frame =
        calculateFrame
         centiseconds
         aps.startOfCurrentAnimation
         animation.framerate
      in
      if frame==animation.numFrames
      then
       {aps|commandQue<-cs
           ,startOfCurrentAnimation<-centiseconds
           ,previousFrame<-0-1
           ,previousRender<-Nothing}
       |> processTick centiseconds 
      else
       case (aps.previousFrame==frame,aps.previousRender) of
        (True,Just render) -> {aps|renderedFrames<-render::aps.renderedFrames}
        _ ->  let render = animation.render frame
              in
              {aps|previousFrame <- frame
                  ,previousRender <- render
                  ,renderedFrames<-render::aps.renderedFrames}
       

  renderConcurrentAnimations: Int -> AnimationPlayerState a -> AnimationPlayerState a
  renderConcurrentAnimations centiseconds aps =
   let
    renderCAPairs = justs <| map (renderConcurrentAnimation centiseconds) aps.concurrentAnimations
    newConcurrentAnimations = map snd renderCAPairs
    renderings = map fst renderCAPairs
   in
   {aps|concurrentAnimations<-}

  renderConcurrentAnimation: Int -> ConcurrentAnimation a -> Maybe (a,ConcurrentAnimation a)
  renderConcurrentAnimation centiseconds ca =
   let
    frame = calculateFrame centiseconds ca.start ca.animation
   in
   case (ca.previousFrame==frame,ca.animation.numframes==frame,ca.previousRender) of
    (True,_,Just render) -> 
     Just
      (render
      ,ca)
    (_,True,_) -> Nothing
    _ ->
     let
      render=ca.animation.render frame
     in
     Just
      (render
      ,{ca|animation={ca.animation|previousFrame  <- frame
                                  ,previousRender <- Just render}})
   

  tics = every <| 10*millisecond
 in
  .renderedFrames <~
   foldp
    processTickOrQueCommand
    initialAnimationPlayerState
    <| (,) <~ count tics ~ animationQueCommandS












{-Takes the number of centiseconds since the start of time, the start time of the animation animation and returns the frame number. -}
calculateFrame: Int -> Int -> Int -> Int
calculateFrame centiseconds start framerate =
 let
  centisecondsPerFrame = div 100 framerate
 in
 div (centiseconds-start) centisecondsPerFrame



data EventMarked a = BEvent a | NoEvent a
eventMarkA: a -> Signal a -> Signal b -> Signal (EventMarked a)
eventMarkA aInit sa sb =
 let
  alternate _ oldAlternator = not oldAlternator
  alternatorS = foldp alternate True sb

  alternatorAndAS = (,) <~ alternatorS ~ sa

  markEvents (a,newAlternator) (a,oldAlternator) =
   if newAlternator == oldAlternator
   then NoEvent a
   else BEvent a
  
 in
 fst <~ foldp markEvents (aInit,True) alternatorAndAS
