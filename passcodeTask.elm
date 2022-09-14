import Array

myShapes model =
  [ rect 200 200 |> filled black |> makeTransparent 0.75
  , drawBase model 1 |> move (45, 0)
  , drawBase model 2 |> move (-45, 0)
  ]

pattern totalCorrect = Array.fromList (List.take (totalCorrect + 1) [5,7,2,1,9])
correctPos model = maybeCancel (Array.get model.roundCorrect (pattern model.totalCorrect))


maybeCancel : Maybe number -> number
maybeCancel num = 
  case num of 
    (Just n) -> 
      n
    (_) -> 
      0
    
drawBase model version = group
  [ rect 80 90 |> filled grey
  , rect 80 90 |> outlined (solid 1) black
  , if version == 1 then drawButtons model (-20, 10) 
    else drawDisplay (maybeCancel (Array.get (floor(model.startTime/30)) (pattern model.totalCorrect)))
  , drawLightCircles (if version == 1 then model.roundCorrect else model.totalCorrect) model.btnClr
  ] 

drawLightCircles numCorrect btnClr = group
  [ drawCircles (numCorrect)     btnClr (-30,30)
  , drawCircles (numCorrect - 1) btnClr (-15,30)
  , drawCircles (numCorrect - 2) btnClr (0,30)
  , drawCircles (numCorrect - 3) btnClr (15,30)
  , drawCircles (numCorrect - 4) btnClr (30,30)
  ]


drawButtons model (posX,posY) = group
  [ drawButton model 1 (posX      , posY)
  , drawButton model 2 (posX + 20 , posY)
  , drawButton model 3 (posX + 40 , posY)
  , drawButton model 4 (posX      , posY - 20)
  , drawButton model 5 (posX + 20 , posY - 20)
  , drawButton model 6 (posX + 40 , posY - 20)
  , drawButton model 7 (posX      , posY - 40)
  , drawButton model 8 (posX + 20 , posY - 40)
  , drawButton model 9 (posX + 40 , posY - 40)
  ]

drawButton model buttonPos (posX, posY) = group
  [ rect 15 15 |> filled (if (floor(model.startTime/30) > model.totalCorrect) then lightGray else darkGray) |> move (posX, posY)
  |> notifyTap (if (floor(model.startTime/30) > model.totalCorrect) then 
  (if (correctPos model) == buttonPos then (if model.roundCorrect == model.totalCorrect then RightAnswer else NextButton) else WrongAnswer) else NoChange)
  , rect 15 15 |> outlined (solid 1) black |> move (posX, posY)
  ]
  
drawDisplay changeColour = group
  [ drawPanel changeColour 1 (-20, 10)
  , drawPanel changeColour 2 (0, 10)
  , drawPanel changeColour 3 (20, 10)
  , drawPanel changeColour 4 (-20, -10)
  , drawPanel changeColour 5 (0, -10)
  , drawPanel changeColour 6 (20, -10)
  , drawPanel changeColour 7 (-20, -30)
  , drawPanel changeColour 8 (0, -30)
  , drawPanel changeColour 9 (20, -30)
  ]

drawPanel changeColour panelNum (posX, posY) = 
  rect 20 20 |> filled (if changeColour == panelNum then lightBlue else black) |> move (posX, posY)

drawCircles totalCorrect btnClr (posX, posY) = group
  [ circle 5 
  |> filled (if totalCorrect > 0 then green else btnClr)
  |> move (posX,posY)
  , circle 5
  |> outlined (solid 1) black
  |> move (posX,posY)
  ] 


type Msg = Tick Float GetKeyState
         | RightAnswer
         | NextButton
         | WrongAnswer
         | NoChange

type alias Model = { time : Float, startTime: Float, totalCorrect : Int, roundCorrect : Int, btnClr : Color}

update msg model = case msg of
                     Tick t _ -> {model | time = t, startTime = model.startTime + 1 }
                     RightAnswer -> {model | totalCorrect = model.totalCorrect + 1, roundCorrect = 0, startTime = 0, btnClr = darkGrey}
                     NextButton -> {model | roundCorrect = model.roundCorrect + 1, btnClr = darkGrey}
                     WrongAnswer -> {model | totalCorrect = 0, roundCorrect = 0, startTime = 0, btnClr = red}
                     NoChange -> model

init = { time = 0, startTime = 0, totalCorrect = 0, roundCorrect = 0, btnClr = darkGrey}

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)



