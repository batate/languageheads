module LanguageHead where 

import Keyboard
import Mouse
import Random

data State = Play | Pause | GameOver  

type Input = { space:Bool, x:Int, delta:Time, rand:Int }
type Head = { x:Float, y:Float, vx:Float, vy:Float }
type Heads = [Head]
type Player = { x:Float, score:Int }
type Game = { state:State, heads:Heads, player:Player }

defaultHead n = {x=100.0, y=75, vx=60, vy=0.0, img=headImage n }  
defaultGame = { state   = Pause,
                heads   = [], 
                player  = {x=0.0, score=0} }
headImage n = 
  if | n == 0 -> "/img/brucetate.png"
     | n == 1 -> "/img/davethomas.png"
     | n == 2 -> "/img/evanczaplicki.png"
     | n == 3 -> "/img/joearmstrong.png"
     | n == 4 -> "/img/josevalim.png"
bottom = 550

secsPerFrame = 1.0 / 50.0  
delta = inSeconds <~ fps 50

input = sampleOn delta (Input <~ Keyboard.space
                               ~ Mouse.x
                               ~ delta
                               ~ Random.range 0 4 (every secsPerFrame))

main = lift display gameState

gameState = foldp stepGame defaultGame input

stepGame ({space, x, delta} as input) ({state, heads, player} as game) = 
  if | state == Play -> stepGamePlay input game 
     | state == Pause -> stepGamePaused input game
     | otherwise -> stepGameFinished input game

stepGamePlay {space, x, delta, rand} ({state, heads, player} as game) =  
  { game | state <-  stepGameOver x heads
         , heads <- stepHeads heads delta x player.score rand
         , player <- stepPlayer player x heads }

stepGameOver x heads = 
  if allHeadsSafe (toFloat x) heads then Play else GameOver
  
allHeadsSafe x heads = 
  all (\head -> headSafe head x) heads

headSafe head x = 
  head.y < bottom || (abs (head.x - x)) < 50

stepHeads heads delta x score rand =    
  spawnHead score heads rand |> 
  bounceHeads |>
  removeComplete |> 
  moveHeads delta
  
spawnHead score heads rand =   
  if length heads < (score `div` 5000 + 1) && 
     all (\head -> head.x > 107.0) heads
  then defaultHead rand :: heads
  else heads

bounceHeads heads = map bounce heads      

bounce head = 
  { head | vy <- if head.y > bottom && head.vy > 0 
                 then -head.vy * 0.95 
                 else head.vy }

removeComplete heads = filter (\x -> not (complete x)) heads  

complete {x} = x > 750

moveHeads delta heads = map moveHead heads     

moveHead ({x, y, vx, vy} as head) = 
  { head | x <- x + vx * secsPerFrame
         , y <- y + vy * secsPerFrame
         , vy <- vy + secsPerFrame * 400 }

stepPlayer player mouseX heads =     
  { player | score <- stepScore player heads
           , x <- toFloat mouseX }
           
stepScore player heads =   
  player.score + 
  1 + 
  1000 * (length (filter complete heads))

stepGamePaused {space, x, delta} ({state, heads, player} as game) =    
  { game | state <- stepState space state
         , player <- { player |  x <- toFloat x } }    

stepGameFinished {space, x, delta} ({state, heads, player} as game) =   
  if space then defaultGame    
  else { game | state <- GameOver
              , player <- { player |  x <- toFloat x } }

stepState space state = if space then Play else state   

display ({state, heads, player} as game) =   
  let (w, h) = (800, 600)
  in collage w h
       ([ drawSky w h
       , drawRoad w h
       , drawBuilding w h
       , drawPaddle w h player.x
       , drawScore w h player
       , drawMessage w h state] ++ 
       (drawHeads w h heads))
drawSky w h = 
  toForm (image 800 600 "/img/sky.jpg")
  
drawRoad w h =   
  toForm (image 800 100 "/img/street.png") |> 
  moveY (-(half h) + 50)
  
drawBuilding w h =
  toForm (image 100 600 "/img/wall.jpg") |> 
  moveX (-(half w) + 50)

drawHeads w h heads = map (drawHead w h) heads   

drawHead w h head = 
  let x = half w - head.x
      y = half h - head.y
      src = head.img
  in toForm (image 75 75 src) |> 
     move (-x, y) |>
     rotate (degrees (x * 2 - 100))

drawPaddle w h x =   
  toForm (image 100 40 "/img/tramp.png") |> 
  moveX (x - 5 -  half w) |> 
  moveY (-(half h - 15))

half x = toFloat x / 2

drawScore w h player =     
  toForm (fullScore player) |> move (half w - 150, half h - 40)

fullScore player = txt (Text.height 50) (show player.score)

txt f = text . f . monospace . Text.color white . toText

drawMessage w h state =    
  toForm (txt (Text.height 50) (stateMessage state)) |>
  move (50, 50)
  
stateMessage state = 
  if state == GameOver then "Game Over" else "Language Heads"
