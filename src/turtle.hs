-----------------------------------------------------------------------------
-- |
-- Module      :  TurtleGraphics
-- Copyright   :  Joe Jevnik 23.9.2013
-- License     :  GPL v2
-- 
-- Maintainer  :  Joe Jevnik
-- Stability   :  experimental
-- Portability :  requires ImageUtils and devIL.
--
-- An example application of my ImageUtils library.
--
-----------------------------------------------------------------------------

import Control.Applicative ((<$>))
import System.Exit (exitSuccess)
import ImageUtils

-- |Data type for the turtle's state.
data Turtle = Turtle { loc   :: (Int,Int) -- Turtles location on the image.
                     , color :: Int       -- The color channel of the tail.
                     , moves :: Int       -- The amount of moves (errors).
                     , image :: Image     -- The image (default is 500,500).
                     }

-- |The default turtle state.
nEW_TURTLE :: IO Turtle
nEW_TURTLE = return 
             $ Turtle { loc   = (0,0)
                      , color = 0
                      , image = listArray ((0,0,0),(499,499,3)) (repeat 255)
                      , moves = 0
                      }

-- |Initializes devIL and starts with a new turtle.
main :: IO ()
main = ilInit >> recurs nEW_TURTLE >> return ()

-- |The main loop that extracts the commands and parses them into actions.
recurs :: IO Turtle -> IO Turtle
recurs iot = do
    t <- iot
    cs <- words <$> getLine
    recurs $ parse_ln cs t

-- |Parse a line of user input or file input.
parse_ln :: [String] -> Turtle -> IO Turtle
parse_ln cs t
    | head cs == "move"  = (move t (cs!!1) (read $ cs!!2)) 
    | head cs == "color" = color_change t (cs!!1)
    | head cs == "write" = write t (cs!!1)
    | head cs == "start" = return 
                           $ Turtle { loc   = (read (cs!!2),read (cs!!1))
                                    , color = 0
                                    , image = listArray ((0,0,0),(499,499,3)) 
                                              (repeat 255)
                                    , moves = 0
                                    }
    | otherwise = error ("Parse error after " ++ show (moves t) ++ " moves")
                  >> exitSuccess >> nEW_TURTLE

-- |Moves the turtle leaving a trail behind it.
move :: Turtle -> String -> Int -> IO Turtle
move t dir n
    | dir == "up"    = return 
                       $ Turtle { loc   = let (x,y) = loc t in (x,y+n)
                                , color = color t
                                , moves = moves t + 1
                                , image = move_up t n
                                }
    | dir == "down"  = return 
                       $ Turtle { loc   = let (x,y) = loc t in (x,y-n)
                                , color = color t
                                , moves = moves t + 1
                                , image = move_down t n
                                }
    | dir == "left"  = return 
                       $ Turtle { loc   = let (x,y) = loc t in (x-n,y)
                                , color = color t
                                , moves = moves t + 1
                                , image = move_left t n
                                }
    | dir == "right" = return 
                       $ Turtle { loc   = let (x,y) = loc t in (x+n,y)
                                , color = color t
                                , moves = moves t + 1
                                , image = move_right t n
                                }

-- |Draws the move up data to the image.
move_up :: Turtle -> Int -> Image
move_up t n = let (x,y) = loc t
              in (image t)//[(i,if (\(_,_,c) -> c) i == color t
                                  then 255
                                  else 0)
                             | i <- filter (\(y',x',_)-> y' `elem` [y..y+n]
                                                         && x' == x)
                                    (indices (image t))]

-- |Draws the move down data to the image.
move_down :: Turtle -> Int -> Image
move_down t n = let (x,y) = loc t 
                in (image t)//[(i,if (\(_,_,c) -> c) i == color t
                                  then 255
                                  else 0)
                               | i <- filter (\(y',x',_) -> y' `elem` [y-n..y]
                                                            && x' == x)
                                      (indices (image t))]

-- |Draws the move left data to the image.
move_left :: Turtle -> Int -> Image
move_left t n = let (x,y) = loc t 
                in (image t)//[(i,if (\(_,_,c) -> c) i == color t
                                  then 255
                                  else 0)
                               | i <- filter (\(y',x',_) -> x' `elem` [x-n..x]
                                                            && y' == y)
                                      (indices (image t))]

-- |Draws the move right data to the image.
move_right :: Turtle -> Int -> Image
move_right t n = let (x,y) = loc t 
                 in (image t)//[(i,if (\(_,_,c) -> c) i == color t
                                  then 255
                                  else 0) 
                                | i <- filter (\(y',x',_) -> x' `elem` [x..x+n]
                                                             && y' == y)
                                       (indices (image t))]

-- |Changes the color channel of the tail of the turtle.
color_change :: Turtle -> String -> IO Turtle
color_change t "red"   = return t { color = 0 }
color_change t "green" = return t { color = 1 }
color_change t "blue"  = return t { color = 2 }
color_change t "alpha" = return t { color = 3 }
color_change t _       = error "Not a valid color" >> return t

-- |Write's the turtles image to a file named fl.
write :: Turtle -> FilePath -> IO Turtle
write t fl = writeImage fl (image t) >> exitSuccess >> nEW_TURTLE
