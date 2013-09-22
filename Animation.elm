{- Public domain - Creative Commons ZERO
   http://creativecommons.org/publicdomain/zero/1.0/
   Timothy Hobbs                         -}

module Animation where










{- Animations produce values which change over time -}

type Animation a =
  {numFrames: Int
  ,framerate: Int
  ,render: Int -> a {- frame -> a -}
  }

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
  
commandMode: Command a -> Mode
commandMode com =
 case com of
  PlayAnimation v -> v.mode
  ClearCommandQue v -> v.mode
  ClearCommandQueAnd v -> v.mode
  SetMode v -> v.mode
  AddConcurrentAnimation v -> v.mode
  AddConcurrentAnimationImediately v -> v.mode


















type AnimationPlayerState a =
  {currentMode: Mode
  ,commandQue: [Command a]
  ,startOfCurrentAnimation: Centiseconds
  ,previousFrame: Int
  ,previousRender: Maybe a
  ,concurrentAnimations: [ConcurrentAnimation a]
  ,renderedFrames: [a]}




initialAnimationPlayerState: AnimationPlayerState a
initialAnimationPlayerState =
  {currentMode=AnyMode
  ,commandQue=[]
  ,startOfCurrentAnimation=0
  ,previousFrame=0-1
  ,previousRender=Nothing
  ,concurrentAnimations=[]
  ,renderedFrames=[]}

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

--processTick: Centiseconds -> AnimationPlayerState a -> AnimationPlayerState a
processTick now aps
 =  aps
 |> processNonAnimationCommands now
 |> renderCurrentAnimation now
 |> renderConcurrentAnimations now



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
     (SetMode v::cs) -> {aps|currentMode<-v.newMode
                            ,commandQue<-cs} 
     (AddConcurrentAnimation v::cs)->
       addConcurrentAnimation aps now v.animation)
 |> processNonAnimationCommands now

currentAnimation: AnimationPlayerState a  -> Animation a
currentAnimation aps =
 case aps.commandQue of
  (PlayAnimation{animation}::cs) ->
   animation

nextCommands:  AnimationPlayerState a  -> [Command a]
nextCommands aps =
 case aps.commandQue of
  (PlayAnimation{animation}::cs) ->
   cs

--processEndOfAnimation: AnimationPlayerState a -> Centiseconds -> AnimationPlayerState a
processEndOfAnimation aps now =
  {aps|commandQue<-nextCommands aps
      ,startOfCurrentAnimation<-now
      ,previousFrame<-0-1
      ,previousRender<-Nothing}
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

foldAnimationPlayerState: Signal (Command a) -> Signal (AnimationPlayerState a)
foldAnimationPlayerState animationQueCommandS =
 foldp
  processTickOrQueCommand
  initialAnimationPlayerState
  <| (,) <~ count tics ~ eventMarkA (SetMode {mode=AnyMode,newMode=AnyMode}) animationQueCommandS tics



{- An animation Player takes a stream of que commands and reacts to them. -}
animationPlayer: Signal (Command a) -> Signal [a]
animationPlayer animationQueCommandS =
 extractRenderedFrames <| foldAnimationPlayerState animationQueCommandS







type Centiseconds = Int

{-Takes the number of centiseconds since the start of time, the start time of the animation animation and returns the frame number. -}
calculateFrame: Centiseconds -> Centiseconds -> Int -> Int
calculateFrame now start framerate =
 let
  centisecondsPerFrame = div 100 framerate
 in
 div (now-start) centisecondsPerFrame





















data EventMarked a = BEvent a | NoEvent a

mkNoEvent: a -> Bool -> (EventMarked a, Bool)
mkNoEvent a alternator = (NoEvent a,alternator)

mkBEvent: a -> Bool -> (EventMarked a, Bool)
mkBEvent a alternator = (BEvent a,alternator)

alternate: a -> Bool -> Bool
alternate _ oldAlternator = not oldAlternator

alternatorS: Signal b -> Signal Bool
alternatorS sb = foldp alternate True sb

eventMarkA: a -> Signal a -> Signal b -> Signal (EventMarked a)
eventMarkA aInit sa sb =
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
  foldInit = (NoEvent aInit,True)
  
 in
 fst <~ foldp markEvents foldInit alternatorAndAS
