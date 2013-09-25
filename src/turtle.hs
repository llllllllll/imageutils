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
                     , color :: Color     -- The color of the tail.
                     , comln :: Int       -- The amount of commands (errors).
                     , image :: Image     -- The image (default is 500,500).
                     }

-- |The default turtle state.
nEW_TURTLE :: IO Turtle
nEW_TURTLE = return 
             $ Turtle { loc   = (0,0)
                      , color = bLACK
                      , image = listArray ((0,0,0),(499,499,3)) (repeat 255)
                      , comln = 1
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
    | null cs           = return $ t { comln = comln t + 1 } -- Empty lines.
    | head cs == "move"  = (move t (cs!!1) (read $ cs!!2))
                           >>= (\t' -> return $ t' { comln = comln t + 1 })
    | head cs == "color" = color_change t (cs!!1)
                           >>= (\t' -> return $ t' { comln = comln t + 1 })
    | head cs == "write" = write t (cs!!1)
    | head cs == "start" = return 
                           $ Turtle { loc   = (read (cs!!2),read (cs!!1))
                                    , color = bLACK
                                    , image = listArray ((0,0,0),(499,499,3)) 
                                              (repeat 255)
                                    , comln = comln t + 1
                                    }
    | head cs == "--"   = return $ t { comln = comln t + 1 } -- Comments.
    | otherwise = error ("Parse error on line " ++ show (comln t) 
                         ++ ": exiting!")
                  >> exitSuccess >> nEW_TURTLE

-- |Moves the turtle leaving a trail behind it.
move :: Turtle -> String -> Int -> IO Turtle
move t dir n
    | dir == "up"    = return 
                       $ t { loc   = let (x,y) = loc t in (x,y+n)
                           , image = move_up t n
                           }
    | dir == "down"  = return 
                       $ t { loc   = let (x,y) = loc t in (x,y-n)
                           , image = move_down t n
                           }
    | dir == "left"  = return 
                       $ t { loc   = let (x,y) = loc t in (x-n,y)
                           , image = move_left t n
                           }
    | dir == "right" = return 
                       $ t { loc   = let (x,y) = loc t in (x+n,y)
                           , image = move_right t n
                           }

-- |Draws the move up data to the image.
move_up :: Turtle -> Int -> Image
move_up t n = let (x,y) = loc t
                  img  = image t
              in draw_seg (x,y) (x,y+n) (color t) img   

-- |Draws the move down data to the image.
move_down :: Turtle -> Int -> Image
move_down t n = let (x,y) = loc t
                    img  = image t
                in draw_seg (x,y) (x,y-n) (color t) img  

-- |Draws the move left data to the image.
move_left :: Turtle -> Int -> Image
move_left t n = let (x,y) = loc t
                    img  = image t
                in draw_seg (x,y) (x-n,y) (color t) img  

-- |Draws the move right data to the image.
move_right :: Turtle -> Int -> Image
move_right t n = let (x,y) = loc t
                     img  = image t
                 in draw_seg (x,y) (x+n,y) (color t) img  

-- |Changes the color channel of the tail of the turtle.
color_change :: Turtle -> String -> IO Turtle
color_change t "red"   = return t { color = rED      }
color_change t "green" = return t { color = gREEN    }
color_change t "blue"  = return t { color = bLUE     }
color_change t "black" = return t { color = bLACK    }
color_change t "white" = return t { color = wHITE    }
color_change t str     = return t { color = read str }

-- |Write's the turtles image to a file named fl.
write :: Turtle -> FilePath -> IO Turtle
write t fl = writeImage fl (image t) >> exitSuccess >> nEW_TURTLE
