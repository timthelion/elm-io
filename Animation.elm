{- Public domain - Creative Commons ZERO
   http://creativecommons.org/publicdomain/zero/1.0/
   Timothy Hobbs                         -}

module Animation where



{- Animations are values which change over time -}


data Animation a =
 Animation
  {numFrames: Int
  ,framerate: Int
  ,render: Int -> a {- frame -}
  }











{- How long does an animation take in hundredths of a second? -}
animationTime: Animation -> Int
animationTime a = round <| toFloat a.numFrames / toFloat a.framerate * 100



{- This really isn't a good way to do things, but all the reasonable methods that occured to me require signal loops. I'd really like to use a signal loop to turn animation streams on and off, but I guess I'll just have to use Modes to ignore animation streams instead.-}
type Mode = Int

data AnimationQueCommand a =
   AddAnimation
    {mode: Mode
    ,animation: Animation a}
 | ClearQueAndPlayAnimationImediately
    {mode: Mode
    ,animation: Animation a}
 | SetMode
    {mode: Mode}



data AnimationPlayerState a =
 AnimationPlayerState
  {currentMode: Mode
  ,animationQue: [Animation a]}



{- An animation Player takes a stream of animations and plays them.
   If the current animation ever finishes than the done Bool becomes true. -}
animationPlayer: Signal (AnimationQueCommand a) -> Signal a
animationPlayer animationS =
 let
  framerateS = .framerate <~ animationS

  dynfps = dynamicFPS framerateS

  frameS = countAbutZeroOnB'sArrival dynfps animationS

  done frame numFrames = frame >= numFrames

  renderFrame animation frame =
    (done frame animation.numFrames,animation.render <| min frame animation.numFrames)
 in
  renderFrame <~ frameS ~ animationS














countUpTo x =
  foldp 
   (\_ acc ->
    if acc < x 
     then acc + 1
     else 0)
   0

dynamicFPS: Signal Int -> Signal Int
dynamicFPS framerateS =
 let
  ticksPerSec = 100

  getFrameNumberForSecond frameCountThisSecond framerate
                 =
          frameCountThisSecond
               `div`
         (div ticksPerSec framerate)

  countUpEverySecond =
   countUpTo ticksPerSecond (every <| 10*millisecond)
  
 in
 dropRepeats
  <| getFrameNumberForSecond <~ countUpEverySecond ~ framerateS






countAbutZeroOnB'sArrival: Signal a -> Signal b -> Signal Int
countAbutZeroOnB'sArrival sa sb =
 let
  countButResetOnBEvent (NoEvent acc) = 0
  countButResetOnBEvent (BEvent acc) = acc+1
 in
 foldP countButResetOnBEvent 0 (eventMarkA sa sb)

data EventMarked a = BEvent a | NoEvent a
eventMarkA: a -> Signal a -> Signal b -> Signal (EventMarked a)
eventMarkA aInit sa sb =
 let
  alternate _ oldAlternator = not oldAlternator
  alternatorS = foldp alternate True sb

  pair a b = (a,b)
  alternatorAndAS = pair <~ alternatorS ~ sa

  markEvents (a,newAlternator) (a,oldAlternator) =
   if newAlternator == oldAlternator
   then NoEvent a
   else BEvent a
  
 in
 fst <~ foldp markEvents (aInit,True) alternatorAndAS
