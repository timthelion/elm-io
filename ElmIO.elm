{- Public domain - Creative Commons ZERO
   http://creativecommons.org/publicdomain/zero/1.0/ 
   Timothy Hobbs                         -}
    
module ElmIO where




{- imports -}




import Animation
import Window





{- helper functions -}



l !! i = head <| drop i l 






{-Returns the y cordinate a form should have in order to have its bottom at a certain above the floor of a collage.-}
bottomAt: Int -> Int -> Int -> Float
bottomAt collageHeight goalAltitude formHeight = extremeAt collageHeight goalAltitude formHeight

leftSideAt: Int -> Int -> Int -> Float
leftSideAt collageWidth goalDistance formWidth = extremeAt collageWidth goalDistance formWidth 

{-Returns the cordinate an form should have in order to have its side at a certain distance from the side of a collage.

Negate to flip.-}
extremeAt: Int -> Int -> Int -> Float
extremeAt collageDimension goalDistance formDimension =
 let
  halfDimensionOf int = toFloat int / 2
  halfFormDimension = halfDimensionOf <| formDimension
  halfCollageDimension = halfDimensionOf <| collageDimension
 in
 halfFormDimension-halfCollageDimension+(toFloat goalDistance)














{- slide words -}


slideWords =
 ["Bigger."
 ,"Better."
 ,"Smarter."
 ,"Stronger."
 ," "
 ,"Interoptable."
 ,"Interactive."
 ,"Inteligent."
 ," "
 ,"Universal."
 ,"Pragmatic."
 ,"Fantastic."
 ," "
 ,"Faster."
 ,"Sexier."
 ,"Classier."
 ,"Sassier."]

slideWordElms = map plainText slideWords 

widest: [Element] -> Int
widest elms = maximum <| map widthOf elms

widthOfWidestSlideWord = round <| (toFloat <| widest slideWordElms) * 1.5

scaleFactor: Int -> Int -> Float
{-Scale w2 to be the same as w1-}
scaleFactor w1 w2 = toFloat w1 / toFloat w2

scaleFactorOfSlideWordsBasedOnScreenWidth: Int -> Float
scaleFactorOfSlideWordsBasedOnScreenWidth screenWidth =
 scaleFactor screenWidth widthOfWidestSlideWord

heightOfScaledSlideWords screenWidth
                            =
                        round <|
 scaleFactorOfSlideWordsBasedOnScreenWidth screenWidth
                            *
             (toFloat <| heightOf (head slideWordElms))

{- Given the number of words in the list, the unscaled height of a word and the scaled width of the longest word give the scaled height of the whole list. -}
scaledWordListHeight: Int -> Int -> Int
scaledWordListHeight numWords screenWidth =
 numWords * heightOfScaledSlideWords screenWidth


scaledWidth: Int -> Element -> Int
scaledWidth width elm = round <| scaleFactorOfSlideWordsBasedOnScreenWidth width * (toFloat <| widthOf elm)






slideWordAnimation: Int -> Animation.Animation [Int -> Int -> Form]
slideWordAnimation word =
 let

  numFrames' = 25
  numFramesInSlidWordsMovement = 20 
  numFramesInSlidingWordMovement = numFramesInSlidWordsMovement - slidingWordFrameOffset
  slidingWordFrameOffset = 8

  framerate' = 30

  render' frame =
   let
    slidWordsFrame = min frame numFramesInSlidWordsMovement
    slidingWordFrame = max 0 (slidWordsFrame-slidingWordFrameOffset)

    slidWords = take word slideWords
    slidWordsAltitude width =
     round <|
       toFloat (heightOfScaledSlideWords width)
                      *
       (toFloat slidWordsFrame/toFloat numFramesInSlidWordsMovement)

    slidWordElms = take word slideWordElms
    slidWordsElm = flow down slidWordElms

    slidWordsForm width height
      =  moveY
         (bottomAt height (slidWordsAltitude width) <| scaledWordListHeight word width)
      <| moveX
         (leftSideAt width 0 (scaledWidth width slidWordsElm))
      <| scale (scaleFactorOfSlideWordsBasedOnScreenWidth width)
      <| toForm
      <| slidWordsElm


    slidingWordLocation width =
     round <|
      (toFloat width)
             -
     (toFloat slidingWordFrame)*(toFloat width/toFloat numFramesInSlidingWordMovement)

    slidingWord = slideWords !! word
    slidingWordElm = plainText slidingWord

    slidingWordForm width height
     =  moveY
         (bottomAt height 0 <| heightOfScaledSlideWords width)
     <| moveX
         (leftSideAt width (slidingWordLocation width) (scaledWidth width slidingWordElm))
     <| scale (scaleFactorOfSlideWordsBasedOnScreenWidth width)
     <| toForm
     <| slidingWordElm

    curtain width height
     =  gradient (linear (0,(toFloat height)/2) (0,-(toFloat height)/2) [(0,black),(1,white)])
     <| rect (toFloat width) (toFloat height)

   in
   [curtain,slidWordsForm,slidingWordForm]
 in
  Animation.mkAnimation
  {numFrames = numFrames'
  ,framerate = framerate'
  ,render = render'
  }

slideWordAnimations: [Animation.Animation [Int->Int->Form]]
slideWordAnimations
  =
 map slideWordAnimation [0..length slideWords-1]

slideWordsMode = 0

slideWordAnimationPlayCommands: [Animation.Command [Int->Int->Form]]
slideWordAnimationPlayCommands =
 Animation.SetMode {mode=Animation.AnyMode,newMode=Animation.Mode slideWordsMode}::
 map slideWordAnimationPlayCommand slideWordAnimations
 ++
 [goBlackPlayCommand
 ,Animation.Wait{mode=Animation.Mode slideWordsMode,delay=200}
 ,Animation.SetMode{mode=Animation.Mode slideWordsMode,newMode=Animation.Mode sloganMode}]

slideWordAnimationPlayCommand: Animation.Animation [Int->Int->Form] -> Animation.Command [Int->Int->Form]
slideWordAnimationPlayCommand animation =
 Animation.PlayAnimation {mode=Animation.Mode slideWordsMode,animation=animation}














 {- Go black -}
goBlackPlayCommand=
 let
  numFrames=40
  framerate=20
  com animation = {mode=Animation.Mode slideWordsMode
                  ,animation= animation}
  animation =
    {framerate=framerate
    ,numFrames=numFrames
    ,render=(\frame-> [(\width height->
     let
      transparency = (toFloat frame)/(toFloat numFrames)
      blackness = rgba 0 0 0 transparency
     in
     filled blackness
     <| rect (toFloat width) (toFloat height))])
    ,stamp=False
    ,id=Nothing}
 in Animation.AddConcurrentAnimationImediately
     <| com animation









{- slogan zoom -}

slogan = "ELM:\nthe future was yesterday\nwelcome to hypertime"

sloganElm c = centered <| Text.color c <|  toText slogan

sloganPos frame numFrames width height =
 let
  goalX =
   if width > 450
   then -(toFloat width)/4
   else (toFloat width)/4
  goalY = 
   if width > 450
   then (toFloat height)/4
   else (toFloat height)/6
  frameFloat = toFloat frame
  animationProcent = frameFloat/(toFloat numFrames)
 in
 (animationProcent*goalX,animationProcent*goalY)



sloganAnimation =
 let
  numFrames'=25
  framerate'=10

  maxSloganScaleFactor width = scaleFactor width (widthOf (sloganElm white))

  sloganScale width frame =
   1+((maxSloganScaleFactor width)-1)*((toFloat <| numFrames'-frame)/(toFloat numFrames'))
  
  sloganForm frame width height
   =  scale (sloganScale width frame)
   <| move (sloganPos frame numFrames' width height)
   <| toForm
   <| (sloganElm white)

  background frame width height
   =  filled black
   <| rect (toFloat width) (toFloat height)

  render' frame = [background frame,sloganForm frame]
 in
 {numFrames=numFrames'
 ,framerate=framerate'
 ,render=render'
 ,id=Nothing
 ,stamp=False}

sloganMode=1

sloganAnimationCommands =
 [Animation.PlayAnimation{mode=Animation.Mode sloganMode,animation=sloganAnimation}
 ,Animation.PlayAnimation{mode=Animation.Mode sloganMode,animation=sloganFadeAnimation}
 ,Animation.SetMode{mode=Animation.Mode sloganMode,newMode=Animation.Mode sunriseMode}]



staticSloganForm c width height
 =  move (sloganPos 25 25 width height)
 <| toForm
 <| (sloganElm c)




{-slogan fade-}
sloganFadeAnimation =
 let
  numFrames'=10
  framerate'=10

  sloganColor frame = rgb (sloganColorPole frame) (sloganColorPole frame) (sloganColorPole frame)
  sloganColorPole frame = round <| 255*((toFloat <| numFrames'-frame)/toFloat numFrames')

  background frame width height
   =  filled black
   <| rect (toFloat width) (toFloat height)

  render' frame = [background frame,staticSloganForm (sloganColor frame)]
 in
 {numFrames=numFrames'
 ,framerate=framerate'
 ,render=render'
 ,id=Nothing
 ,stamp=False}















{- sunrise -}
sunriseMode = 2

sunriseAnimation =
 let
  framerate = 20
  numFrames = 100

  sunPos: Int -> Int -> Int -> (Float,Float)
  sunPos frame width height =
   let
    rad =  (toFloat <| height)/ 2
    angle = turns <| 1.5-(0.25*(toFloat frame/toFloat numFrames))
   in
   (rad * cos angle, rad * sin angle)

  sunriseCenter:Int->Int->(Float,Float)
  sunriseCenter width height =
   ((toFloat width)/2,-(toFloat height)/2)

  sunriseOpacity frame = ((toFloat numFrames)-(toFloat frame))/(toFloat numFrames)

  sunGradient frame = -- Stolen from http://elm-lang.org/edit/examples/Elements/RadialGradient.elm
   radial (0,0) 50 (0,10) 90
         [(0  , rgba  244 242 1 (sunriseOpacity frame)),
          (0.8, rgba  228 199 0 (sunriseOpacity frame)),
          (1  , rgba 228 199 0 0)]
  
  sun frame width height
   =  move (sunriseCenter width height)
   <| move (sunPos frame width height)
   <| gradient (sunGradient frame)
   <| circle 100

   
  sky frame width height
   =  filled (rgba 0 0 0 (sunriseOpacity frame))
   <| rect (toFloat width) (toFloat height)
   
  render frame =
   [sky frame, sun frame]
 in
 Animation.mkAnimation
 {framerate=framerate
 ,numFrames=numFrames
 ,render=render}


sunriseCommands =
 [Animation.AddConcurrentAnimation
   {mode=Animation.Mode sunriseMode
   ,animation=sunriseAnimation}
 ,Animation.SetMode
   {mode=Animation.Mode sunriseMode
   ,newMode=Animation.Mode clockMode}]











{- clock -}

clockSeconds = {- NOTE! MUST BE EXACTLY 60 ELEMENTS LONG! -}
 [("ELM","Experience Limitless Momentum")
 ,("Explore","new possibilities") 
 ,("Experience","the joy") 
 ,("Establishes","a new platform")
 ,("Eliminates","the hastle of expressiveness")
 ,("Expands","your mind")
 ,("Connect","through color movement and light")
 ,("Custom","everything puts you in control")
 ,("Create","what others only imagine")
 ,("Complex","some write apps, we write applications")
 ,("Compile","twice, cut once")
 ,("Conceptual","ease, quantum capability")
 ,("Capable","of the most challenging tasks")
 ,("Cathedral","design, bazar development")
 ,("Ceaseless","improvement")
 ,("Central","perspective, global impact")
 ,("Meaningful","real value real code")
 ,("Modular","assemble truely amazing applications")
 ,("Discover","functional reactive professionalism") 
 ,("Design","the way you imagine") 
 ,("Direct","the future with power ELM") 
 ,("Dream","elm lets you sleep soundly at night") 
 ,("Deploy","to the whole world")
 ,("Delightful","just fun")
 ,("Daring","FRP and records syntax")
 ,("Lyrical","the most beautiful code")
 ,("Latest","techniques ensure you're leading the pack")
 ,("Lasting","a belief in the future of JavaScript")
 ,("Love","writing code")
 ,("Loaded","with half a century of science")
 ,("Lower latency","bring things client side")
 ,("Less debugging","more deployment")
 ,("Learn","new ways of seeing the world")
 ,("Solid","when diamond doesn't cut it, try elm")
 ,("Safe","types save lives")
 ,("Sound","accademically and in the real world")
 ,("Imutable","means code you can trust")
 ,("Intense","expansion without breakage")
 ,("Integrate","web applications anywhere")
 ,("Introduce","unseen simplicity unknown power")
 ,("Wow","never knew it could be easy")
 ,("World changing","be the future, don't wait")
 ,("Web 4.0","modern language features atop JavaScript")
 ,("Generates","EMCAscript hand crafted for your browser")
 ,("Georgeous","algorithmic color support")
 ,("Genereralize","for re-use and value enhancement")
 ,("Runs everywhere","we mean EVERYWHERE")
 ,("React","with unmatched productivity")
 ,("Hotswaping","makes real time development real")
 ,("Signal graphs","put you in a whole new dimension")
 ,("Argot free","no words to learn just GO!")
 ,("Artful","syntax your code is a pleasure to view")
 ,("Allocate","your team more effectively")
 ,("Automatic","a compiler that almost codes for you")
 ,("Banish","errors with resiliant models")
 ,("Biologically","inspired automaton goodness")
 ,("Bootstrap","your reactive journey")
 ,("Bring","new perspective with a new paradigm")
 ,("Panoramic","understanding of expression")
 ,("Open source","assurance of quality from all angles")
 ]

clockWords = map fst clockSeconds
clockWordElms = map plainText clockWords
clockWordForms' = map toForm clockWordElms

numberedClockWordForms: [(Form,Int)]
numberedClockWordForms = zip clockWordForms' <| tail <| scanl (\f c->c+1) 0 clockWordForms'

clockForms time frame =
 [sloganBackground time
 ,staticSloganForm black
 ,elmLangWebPageLink]++
 clockWordForms time frame


activeSlogan time = 59 - (rem (ceiling <| inSeconds time) 60)

currentSlogan time width height =
 let
  thisSecond = 
   clockSeconds !! activeSlogan time
  keyWord
   =  righted
   <| Text.color white
   <| bold 
   <| toText
   <| (\kw->kw++" ")
   <| fst thisSecond
  expression
   =  righted
   <| Text.color white
   <| toText
   <| snd thisSecond
 in
    toForm
 <| flow right [keyWord, expression]

sloganBackground time width height
 =   filled blue
 <|  rect (toFloat width)
          (toFloat <| heightOf <| head clockWordElms)

clockWordForms time frame = map (placeWord time frame) numberedClockWordForms

placeWord time frame (form,n) =
 if (n-1 ==activeSlogan time)
 then currentSlogan time
 else placeWord' time frame (form,n)

placeWord': Time -> Int -> (Form,Int) -> Int -> Int -> Form
placeWord' time frame (form,n) width height =
 let
   --framesInMovement = 30
   --movementFrame = max (frame - 70) 0
   --partialDeg = (framesInMovement/(framesInMovement-(toFloat movementFrame))) - 1
   degs  = 90 - 6 * (toFloat (ceiling <| inSeconds time)+toFloat n) + 90 -- partialDeg
   rotDegs = degs + 180
   angle = degrees degs -- Stolen! http://elm-lang.org/edit/examples/Intermediate/Clock.elm
   len = max 250 <| ((toFloat<|min width height)/2) - 50
  in
    rotate (degrees rotDegs)
 <| moveX (toFloat width / 2)
 <| move (len * cos angle, len * sin angle)
 <| form

vector len angle = (len * cos angle, len * sin angle)

clockMode=3

makeClockCommand time =
 Animation.ClearCommandQueAnd
 {mode=Animation.Mode clockMode
 ,command=
 Animation.PlayAnimation
  {mode=Animation.Mode clockMode
  ,animation=
   Animation.mkAnimation {framerate=100-- Why is this more efficient than a framerate of 1?
   ,numFrames=1
   ,render = clockForms time
   }
  }
 }

clockAnimationCommandSignal = makeClockCommand <~ every second















elmLangWebPage = link "http://elm-lang.org" <| flow right [image 50 50 "logo.png",plainText "elm-lang.org"]

elmLangWebPageLink width height
 =
 (if width > 450
 then move (-(toFloat width)/4,-(toFloat height)/4)
 else move ((toFloat width)/4,-(toFloat height)/6))
 <| toForm
 <| elmLangWebPage



















leGrandAnimationPlayer =
 Animation.animationPlayer
 (slideWordAnimationPlayCommands
 ++ sloganAnimationCommands
 ++ sunriseCommands)
 clockAnimationCommandSignal

main = (\(w,h) forms-> collage w h <| map (\form->form w h) <|  concat forms) <~ Window.dimensions ~ leGrandAnimationPlayer
