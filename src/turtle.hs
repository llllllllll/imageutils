-----------------------------------------------------------------------------
-- |
-- Module      :  TurtleGraphics
-- Copyright   :  Joe Jevnik 27.9.2013
-- License     :  GPL v2
--
-- Maintainer  :  Joe Jevnik
-- Stability   :  experimental
-- Portability :  requires ImageUtils and devIL.
--
-- An example application of my ImageUtils library.
--
-----------------------------------------------------------------------------

import Text.Read (readMaybe)
import Control.Applicative ((<$>))
import System.Exit (exitSuccess,exitFailure)
import ImageUtils

-- |Data type for the turtle's state.
data Turtle = Turtle { loc   :: (Int,Int) -- Turtles location on the image.
                     , color :: Color     -- The color of the tail.
                     , comln :: Int       -- The amount of commands (erroring).
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

-- |Initializes devIL and starts recursing with a new turtle.
main :: IO ()
main = ilInit >> recurs nEW_TURTLE >> return ()

-- |The main loop that extracts the commands and parses them into actions.
recurs :: IO Turtle -> IO Turtle
recurs iot = do
    t  <- iot
    recurs $ (words <$> getLine) >>= parse_ln t

-- |Parse a line of user input or file input.
parse_ln :: Turtle -> [String] -> IO Turtle
parse_ln t cs
    | null cs            = return $ t { comln = comln t + 1 } -- Empty lines.
    | head cs == "move"
      && null (tail cs)  =  error ("Parse error on line: " ++ show (comln t)
                                   ++ ": direction needed: up down left right")
    | head cs == "move"  = (move t (cs!!1) (read $ cs!!2))
                           >>= (\t' -> return $ t' { comln = comln t + 1 })
    | head cs == "color" = color_change t (cs!!1)
                           >>= (\t' -> return $ t' { comln = comln t + 1 })
    | head cs == "write" = write t (cs!!1)
    | head cs == "new" && (null (tail cs) || null (tail (tail cs)))
                         = error ("Parse error on line: " ++ show (comln t)
                                  ++ ": missing paramaters to new: x y")
                           >> exitFailure >> nEW_TURTLE
    | head cs == "new"   = new t (cs!!1) (cs!!2)
    | head cs == "--"    = return $ t { comln = comln t + 1 } -- Comments.
    | otherwise = error ("Parse error on line: " ++ show (comln t)
                         ++ ": command not recognized: '" ++ head cs ++ "'")
                  >> exitFailure >> nEW_TURTLE

-- |Moves the turtle leaving a trail behind it.
move :: Turtle -> String -> Int -> IO Turtle
move t dir n
    | dir == "up"    = let (x,y) = loc t
                       in return $ t { loc   = (x,y+n)
                                     , image = draw_seg (x,y) (x,y+n) (color t)
                                               (image t)
                                     }
    | dir == "down"  = let (x,y) = loc t
                       in return $ t { loc   = (x,y-n)
                                     , image = draw_seg (x,y) (x,y-n) (color t)
                                               (image t)
                                     }
    | dir == "left"  = let (x,y) = loc t
                       in return  $ t { loc   = (x-n,y)
                                      , image = draw_seg (x,y) (x-n,y) (color t)
                                                (image t)
                                      }
    | dir == "right" = let (x,y) = loc t
                       in return  $ t { loc   = (x+n,y)
                                      , image = draw_seg (x,y) (x+n,y) (color t)
                                                (image t)
                                      }
    | otherwise      = error ("Parse error on line " ++ show (comln t)
                              ++ ": invalid direction: '" ++ dir ++ "'")
                       >> exitFailure >> nEW_TURTLE

-- |Changes the color channel of the tail of the turtle.
color_change :: Turtle -> String -> IO Turtle
color_change t "red"   = return t { color = rED      }
color_change t "green" = return t { color = gREEN    }
color_change t "blue"  = return t { color = bLUE     }
color_change t "black" = return t { color = bLACK    }
color_change t "white" = return t { color = wHITE    }
color_change t str     = case readMaybe str :: Maybe (Word8,Word8,Word8) of
                             Nothing -> error ("Parse error on line "
                                               ++ show (comln t)
                                               ++ ": invalid color: '" ++ str
                                               ++ "': use '(r,g,b)'")
                                        >> exitFailure >> nEW_TURTLE
                             Just c -> return t { color = c }

-- |News a new turtle on white image of size x y.
new :: Turtle -> String -> String -> IO Turtle
new t xstr ystr = case ( readMaybe xstr :: Maybe Int
                       , readMaybe ystr :: Maybe Int
                       )
                  of
                    (Nothing,Nothing) -> error ("Parse error on line "
                                                ++ show (comln t)
                                                ++ ": bad new size "
                                                ++ "arguments: '" ++ xstr
                                                ++ "' '" ++ ystr ++ "'")
                                         >> exitFailure >> nEW_TURTLE
                    (Nothing,_)       -> error ("Parse error on line "
                                              ++ show (comln t)
                                                ++ ": bad new x size "
                                                ++ "argument: '" ++ xstr ++ "'")
                                         >> exitFailure >> nEW_TURTLE
                    (_,Nothing)       -> error ("Parse error on line "
                                                ++ show (comln t)
                                                ++ ": bad new y argument: '"
                                                ++ ystr ++ "'")
                                         >> exitFailure >> nEW_TURTLE
                    (Just x,Just y)   -> return
                                         $ Turtle { loc   = (0,0)
                                                  , color = bLACK
                                                  , image = listArray ((0,0,0)
                                                                      ,(y,x,3))
                                                            (repeat 255)
                                                  , comln = comln t + 1
                                                  }

-- |Write's the turtles image to a file named fl.
write :: Turtle -> FilePath -> IO Turtle
write t fl
    | dropWhile (/='.') fl `elem` [ ".bmp"
                                  , ".dds"
                                  , ".exr"
                                  , ".h"
                                  , ".jpg"
                                  , ".jp2"
                                  , ".pal"
                                  , ".pcx"
                                  , ".png"
                                  , ".pbm"
                                  , ".pgm"
                                  , ".pgm"
                                  , ".pnm"
                                  , ".psd"
                                  , ".raw"
                                  , ".sgi"
                                  , ".bw"
                                  , ".rgb"
                                  , ".rgba"
                                  , ".tga"
                                  , ".tif"
                                  , ".vtf"
                                  ]
                = writeImage fl (image t)
                  >> exitSuccess >> nEW_TURTLE
    | otherwise = error ("Parse error on line: " ++ show (comln t)
                         ++ ": invalid file format: '" ++ dropWhile (/='.') fl
                         ++ "'")
                  >> exitFailure >> nEW_TURTLE
