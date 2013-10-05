{- Public domain - Creative Commons ZERO
   http://creativecommons.org/publicdomain/zero/1.0/
   Timothy Hobbs                         -}

module Animation where










{- Animations produce values which change over time -}

type Animation a =
  {numFrames: Int
  ,framerate: Int
  ,render: Int -> a {- frame -> a -}
  ,stamp: Bool
  ,id: Maybe Int
  }

type SimpleAnimation a =
  {numFrames: Int
  ,framerate: Int
  ,render: Int -> a {- frame -> a -}}

mkAnimation: SimpleAnimation a -> Animation a
mkAnimation simple =
 {numFrames=simple.numFrames
 ,framerate=simple.framerate
 ,render=simple.render
 ,stamp=False
 ,id=Nothing}

type ConcurrentAnimation a =
  {start: Centiseconds
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
 | Wait
    {mode: Mode
    ,delay: Centiseconds}
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
 | DeleteStamp
    {mode: Mode
    ,id: Maybe Int}
 | DeleteConcurrentAnimation
    {mode: Mode
    ,id: Maybe Int}
  
commandMode: Command a -> Mode
commandMode com =
 case com of
  PlayAnimation v -> v.mode
  ClearCommandQue v -> v.mode
  ClearCommandQueAnd v -> v.mode
  SetMode v -> v.mode
  AddConcurrentAnimation v -> v.mode
  AddConcurrentAnimationImediately v -> v.mode









type Stamp a =
 {id: Maybe Int
 ,renderedFrame: a}










type AnimationPlayerState a =
  {currentMode: Mode
  ,commandQue: [Command a]
  ,startOfCurrentAnimation: Centiseconds
  ,previousFrame: Int
  ,previousRender: Maybe a
  ,concurrentAnimations: [ConcurrentAnimation a]
  ,renderedFrames: [a]
  ,stamps: [Stamp a]}




initialAnimationPlayerState: [Command a] -> AnimationPlayerState a
initialAnimationPlayerState initialCommands =
  {currentMode=AnyMode
  ,commandQue=initialCommands
  ,startOfCurrentAnimation=0
  ,previousFrame=0-1
  ,previousRender=Nothing
  ,concurrentAnimations=[]
  ,renderedFrames=[]
  ,stamps=[]}

processTickOrQueCommand: (Centiseconds,EventMarked (Command a)) -> AnimationPlayerState a -> AnimationPlayerState a
processTickOrQueCommand (now,evc) aps =
 case evc of
  NoEvent com ->
   if commandMode com == aps.currentMode
                      ||
      commandMode com == AnyMode
                      ||
      aps.currentMode == AnyMode
   then
    case com of
     ClearCommandQue _ -> {aps|commandQue <- []}
     ClearCommandQueAnd v -> {aps|commandQue <- [v.command]}
     AddConcurrentAnimationImediately v ->
      addConcurrentAnimation aps now v.animation
     _ -> {aps|commandQue <- aps.commandQue ++ [com]}
   else 
    aps
  BEvent _ ->
      {aps|renderedFrames<-[]}
   |> processTick now
  Uninitialized -> aps

--processTick: Centiseconds -> AnimationPlayerState a -> AnimationPlayerState a
processTick now aps
 =  aps
 |> processNonAnimationCommands now
 |> (\aps' -> if thereIsAnAnimation aps' then renderCurrentAnimation now aps' else usePreviousRender aps')
 |> renderConcurrentAnimations now
 |> stampStamps

stampStamps: AnimationPlayerState a -> AnimationPlayerState a
stampStamps aps =
 {aps|renderedFrames<-aps.renderedFrames++map (\stamp->stamp.renderedFrame) aps.stamps}

usePreviousRender: AnimationPlayerState a -> AnimationPlayerState a
usePreviousRender aps =
 case aps.previousRender of
  Just render -> addRender aps render
  Nothing -> aps

addConcurrentAnimation: AnimationPlayerState a -> Centiseconds -> Animation a -> AnimationPlayerState a 
addConcurrentAnimation aps now animation =
  let
   --newConcurrentAnimation: ConcurrentAnimation a
   newConcurrentAnimation =
    {start=now
    ,previousFrame=0-1
    ,previousRender=Nothing
    ,animation=animation} 
  in
  {aps|concurrentAnimations <- newConcurrentAnimation :: aps.concurrentAnimations}

--processNonAnimationCommands:
-- Centiseconds -> AnimationPlayerState a -> AnimationPlayerState a
processNonAnimationCommands now aps
 =  (case aps.commandQue of
     (ClearCommandQue _::cs)    -> {aps|commandQue <- []}
     (ClearCommandQueAnd v::cs) -> {aps|commandQue <- [v.command]}
     (AddConcurrentAnimationImediately v::cs) ->
      addConcurrentAnimation aps now v.animation |>
      (\aps->{aps|commandQue<-cs}) |> processNonAnimationCommands now

     (SetMode v::cs) -> {aps|currentMode<-v.newMode
                            ,commandQue<-cs} |> processNonAnimationCommands now
     (AddConcurrentAnimation v::cs) ->
       addConcurrentAnimation aps now v.animation
       |> (\aps->{aps|commandQue<-cs}) 
       |> processNonAnimationCommands now
     (Wait v::cs) ->
       let
        waitAnimation=
         case aps.previousRender of
          Just render ->
           {numFrames=v.delay
           ,framerate=100
           ,render=(\_->render)
           ,id=Nothing
           ,stamp=False}
       in
       {aps|commandQue<-PlayAnimation {mode=v.mode,animation=waitAnimation}::cs}
     (DeleteStamp v::cs) ->
       {aps|stamps<-filter (\stamp->stamp.id/=v.id) aps.stamps}
     (DeleteConcurrentAnimation v::cs) ->
       {aps|concurrentAnimations<-filter (\concurrentAnimation->concurrentAnimation.animation.id/=v.id) aps.concurrentAnimations}
     _ -> aps)
 
thereIsAnAnimation: AnimationPlayerState a -> Bool
thereIsAnAnimation aps =
 case aps.commandQue of
  PlayAnimation{animation}::cs -> True
  [] -> False

currentAnimation: AnimationPlayerState a  -> Animation a
currentAnimation aps =
 case aps.commandQue of
  PlayAnimation{animation}::cs -> animation

nextCommands:  AnimationPlayerState a  -> [Command a]
nextCommands aps =
 case aps.commandQue of
  (PlayAnimation{animation}::cs) ->
   cs

--processEndOfAnimation: AnimationPlayerState a -> Centiseconds -> AnimationPlayerState a
processEndOfAnimation aps now =
  let
   endingAnimation = currentAnimation aps
   newStamps =
    if endingAnimation.stamp
    then
     case aps.previousRender of
      Just render -> {id=endingAnimation.id,renderedFrame=render}::aps.stamps
    else aps.stamps
  in
  {aps|commandQue<-nextCommands aps
      ,startOfCurrentAnimation<-now
      ,previousFrame<-0-1
      ,stamps<-newStamps}
  |> processTick now

addRender aps render = {aps|renderedFrames<-render::aps.renderedFrames}

renderCurrentAnimation' aps animation frame =
  let
   render = animation.render frame
  in
   {aps|previousFrame  <- frame
       ,previousRender <- Just render
       ,renderedFrames <-render::aps.renderedFrames}

continueRenderingThisAnimation: AnimationPlayerState a -> Animation a -> Int -> AnimationPlayerState a
continueRenderingThisAnimation aps animation frame =
 case (aps.previousFrame==frame,aps.previousRender) of
  (True,Just render) -> addRender aps render {- from cache -}
  (True,Nothing)     -> aps --Shouldn't happen
  _                  -> renderCurrentAnimation' aps animation frame {- anew -}

--renderCurrentAnimation: Centiseconds -> AnimationPlayerState a -> AnimationPlayerState a
renderCurrentAnimation now aps =
 let
  animation = currentAnimation aps
  frame =
   calculateFrame
    now
    aps.startOfCurrentAnimation
    animation.framerate
 in
 if frame==animation.numFrames
 then processEndOfAnimation aps now
 else continueRenderingThisAnimation aps animation frame

--renderConcurrentAnimations: Centiseconds -> AnimationPlayerState a -> AnimationPlayerState a
renderConcurrentAnimations now aps =
 let

  renderCAPairs =  justs
                <| map
                    (renderConcurrentAnimation now)
                    aps.concurrentAnimations

  newConcurrentAnimations = map snd renderCAPairs

  renderings = map fst renderCAPairs
 in
 {aps|concurrentAnimations <- newConcurrentAnimations
     ,renderedFrames       <- aps.renderedFrames++renderings}

--renderConcurrentAnimation: Centiseconds -> ConcurrentAnimation a -> Maybe (a,ConcurrentAnimation a)
renderConcurrentAnimation now ca =
 let
--  animation: Animation a
  animation = ca.animation
  frame = calculateFrame now ca.start animation.framerate
  fromCache render = Just (render ,ca)
  frameChange = ca.previousFrame /= frame
  animationEnd = animation.numFrames == frame
  renderAnew =
   let
    render=animation.render frame
     
    ca'={ca|previousFrame  <- frame
           ,previousRender <- Just render}
   in
   Just (render,ca')
 
 in
 case (frameChange,animationEnd,ca.previousRender) of
  (False,_,Just render) -> fromCache render
  (_,True,_) -> Nothing
  _ -> renderAnew

tics: Signal Time
tics = every <| 10*millisecond

extractRenderedFrames: Signal (AnimationPlayerState a) -> Signal [a]
extractRenderedFrames apsS = .renderedFrames <~ apsS

foldAnimationPlayerState: [Command a] -> Signal (Command a) -> Signal (AnimationPlayerState a)
foldAnimationPlayerState initialCommands animationQueCommandS =
 foldp
  processTickOrQueCommand
  (initialAnimationPlayerState initialCommands)
  <| (,) <~ count tics ~ eventMarkA animationQueCommandS tics



{- An animation Player takes a stream of que commands and reacts to them. -}
animationPlayer: [Command a] -> Signal (Command a) -> Signal [a]
animationPlayer initialCommands animationQueCommandS =
 extractRenderedFrames <| (foldAnimationPlayerState initialCommands) animationQueCommandS







type Centiseconds = Int

{-Takes the number of centiseconds since the start of time, the start time of the animation animation and returns the frame number. -}
calculateFrame: Centiseconds -> Centiseconds -> Int -> Int
calculateFrame now start framerate =
 let
  centisecondsPerFrame = div 100 framerate
 in
 div (now-start) centisecondsPerFrame





















data EventMarked a = BEvent a | NoEvent a | Uninitialized

mkNoEvent: a -> Bool -> (EventMarked a, Bool)
mkNoEvent a alternator = (NoEvent a,alternator)

mkBEvent: a -> Bool -> (EventMarked a, Bool)
mkBEvent a alternator = (BEvent a,alternator)

alternate: a -> Bool -> Bool
alternate _ oldAlternator = not oldAlternator

alternatorS: Signal b -> Signal Bool
alternatorS sb = foldp alternate True sb

eventMarkA: Signal a -> Signal b -> Signal (EventMarked a)
eventMarkA sa sb =
 let
--  alternatorAndAS: Signal (a,Bool)
  alternatorAndAS = (,) <~ sa ~ alternatorS sb

--  markEvents: (a,Bool) -> (EventMarked a,Bool) -> (EventMarked a,Bool)
  markEvents (a,newAlternator) (_,oldAlternator) =
   case newAlternator == oldAlternator of
    True ->
     mkNoEvent a newAlternator
    False ->
     mkBEvent a newAlternator
  
--  foldInit: (EventMarked a, Bool)
  foldInit = (Uninitialized,True)
  
 in
 fst <~ foldp markEvents foldInit alternatorAndAS
