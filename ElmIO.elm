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

   in
   [slidWordsForm,slidingWordForm]
 in
  {numFrames = numFrames'
  ,framerate = framerate'
  ,render = render'
  }

slideWordAnimations: [Animation.Animation [Int->Int->Form]]
slideWordAnimations
  =
 map slideWordAnimation [0..length slideWords-1]

slideWordAnimationPlayCommands: [Animation.Command [Int->Int->Form]]
slideWordAnimationPlayCommands =
 map slideWordAnimationPlayCommand slideWordAnimations

slideWordAnimationPlayCommand: Animation.Animation [Int->Int->Form] -> Animation.Command [Int->Int->Form]
slideWordAnimationPlayCommand animation =
 Animation.PlayAnimation {mode=Animation.AnyMode,animation=animation}











{- slogan zoom -}

slogan = "ELM\nthe future was yesterday\nwelcome to hypertime"

sloganElm = centered <| toText slogan

sloganAnimation =
 let
  numFrames'=20
  framerate'=10
  maxSloganScaleFactor width = scaleFactor width (widthOf sloganElm)
  sloganScale width frame =
   1+((maxSloganScaleFactor width)-1)*((toFloat <| numFrames'-frame)/(toFloat numFrames'))
  sloganForm frame width height
   =  scale (sloganScale width frame)
   <| toForm
   <| sloganElm
  render' frame = [sloganForm frame]
 in
 {numFrames=numFrames'
 ,framerate=framerate'
 ,render=render'}

sloganAnimationCommands =
 [Animation.PlayAnimation{mode=Animation.AnyMode,animation=sloganAnimation}]









{-
{- clock -}

clockMiddle = slogan

clockSecconds = {- NOTE! MUST BE EXACTLY 60 ELEMENTS LONG! -}
 [
 ,("ELM","Experience Limitless Momentum")
 ,("Explore","new possibilities with functional expressivism") 
 ,("Experience","the joy of deploying solid code") 
 ,("Establishes","a new platform for new ideas and new directions")
 ,("Eliminates","the hastle of HTML by going beyond past expressiveness")
 ,("Expands","your mind with unparaleled concurent programming")
 ,("Connect","with customers and clients with cutting edge ")
 ,("Custom","web elements put you in control")
 ,("Create","astounding projects and wow your peers")
 ,("Complex","some write apps, we write applications")
 ,("Complete","solution for everything from finace to home beauty sales")
 ,("Conceptual","ease, quantum capability")
 ,("Capable","of even the most challenging tasks")
 ,("Cathedral","design, bazar development")
 ,("Ceaseless","improvement with each release")
 ,("Central","perspective, global impact")
 ,("Meaningful","bring real value to your customers with real code")
 ,("Modular","fully featured module system lets you build truely amazing applications")
 ,("Discover","the power of functional reactive professionalism") 
 ,("Design","the way you imagine") 
 ,("Direct","the future with the power of ELM") 
 ,("Dream","elm is the first language that lets you sleep soundly at night") 
 ,("Deploy","applications in mere minutes to the whole world")
 ,("Delightful","just fun")
 ,("Daring","cuttin edge FRP and records syntax")
 ,("Lyrical","write the most beautiful code, right in your browser")
 ,("Latest","compiler techniques ensure you're leading the pack")
 ,("Lasting","built with a belief in the future of JavaScript")
 ,("Love","writing code")
 ,("Loaded","with half a century of science")
 ,("Lower latency","by bringing things client side")
 ,("Less debugging","more deployment")
 ,("Learn","new ways of thinking, new ways of seeing the world")
 ,("Solid","when diamond doesn't cut it, try elm")
 ,("Safe","type safety from the get go")
 ,("Sound","accademically reviewed, real world testing")
 ,("Imutable","values mean code you can trust")
 ,("Intense","expansion without headaches or breakage")
 ,("Integrate","your web applications anywhere")
 ,("Introduce","unseen simplicity with unknown power")
 ,("Wow","I never knew it could be this easy")
 ,("World changing","don't just wait for the future, be there")
 ,("Web 4.0","join web 4.0, using modern language features on top of JavaScript")
 ,("Generates","carefully optimized JavaScript code hand crafted for your browser of choice")
 ,("Georgeous","create georgeous websites with Elm's built in algorithmic color support")
 ,("Genereralize","even the most complex algorithms for re-use and value enhancement")
 ,("Runs everywhere","on any device that supports HTML and JavaScript")
 ,("React","to marked demands with unmatched productivity")
 ,("Hotswap","out code for testing and real time development")
 ,("Graphs","of signals put you in a whole new dimension")
 ,("Argot free","no words to learn just GO!")
 ,("Artful","syntax makes your code a pleasure to view")
 ,("Allocate","team reasorces more effectively with a functional modular design")
 ,("Automatically","find bugs with a compiler that almost codes for you")
 ,("Banish","errors with resiliant models")
 ,("Beat","the neighborhood by being in the functional game first")
 ,("Biologically","inspired automaton goodness")
 ,("Bootstrap","your reactive journey with hundreds of enlightening examples")
 ,("Bring","new perspective with a new paradigm")
 ,("Panoramic","understanding of expression througout the full stack")
 ,("Open source","assurance of quality from all angles")
 ]

















-- TODO Add white background.
elmLangWebPage = link "http://elm-lang.org" <| flow right [img "logo.png",plainText "elm-lang.org"]
















-}

leGrandAnimationPlayer = Animation.animationPlayer (slideWordAnimationPlayCommands ++ sloganAnimationCommands) (constant <| Animation.SetMode{mode=Animation.AnyMode,newMode=Animation.AnyMode})

main = (\(w,h) forms-> collage w h <| map (\form->form w h) <|  concat forms) <~ Window.dimensions ~ leGrandAnimationPlayer
