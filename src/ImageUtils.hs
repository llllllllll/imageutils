-----------------------------------------------------------------------------
-- |
-- Module      :  ImageUtils
-- Copyright   :  Joe Jevnik 2013
-- License     :  GPL v2
-- 
-- Maintainer  :  Joe Jevnik
-- Stability   :  stable
-- Portability :  requires devIL
--
-- Operations on image files.
--
-----------------------------------------------------------------------------

module ImageUtils 
    ( module Data.Array.Unboxed
    , module Codec.Image.DevIL
    , Image
    , Color(..)
    -- * Color changes
    , strip           -- :: Color -> Image -> Image
    , (<\\>)          -- :: Image -> Color -> Image
    , filter_img      -- :: Color -> Image -> Image
    , (<#>)           -- :: Image -> Color -> Image
    , edit_brightness -- :: Double -> Image
    , edit_color      -- :: Color -> Double -> Image
    , invert_colors   -- :: Image -> Image
    , invert_color    -- :: Color -> Image
    -- * Orientation
    , flip_vert       -- :: Image -> Image
    , flip_horz       -- :: Image -> Image
    -- * Other
    , merge           -- :: Image -> Image -> Image
    ) where

import Codec.Image.DevIL
import Data.Array.Unboxed

-- |Alias for the type of data returned from readImage.
type Image = UArray (Int,Int,Int) Word8 -- Indices: (R,G,B)

-- |The color channels on in the RGB images.
data Color = Red | Green | Blue | Alpha


-- Testing functions.
main :: IO ()
main = do
    ilInit
    img <- readImage "../ducks.jpg"
    -- writeImage "../ducks_flipped.jpg" (flip_vert img)
    writeImage "../test4.jpg" $ invert_color Red $ invert_color Blue $ invert_color Green img
    -- writeImage "../noblue.jpg" (strip_blue img)


-- -----------------------------------------------------------------------------
-- Color stripping

-- |Strips an Image of all of a primary color.

strip :: Color -> Image -> Image
strip Red   img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x == 0) i]
strip Green img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x == 1) i]
strip Blue  img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x == 2) i]
strip Alpha img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x == 3) i]


-- |An alias of strip.
-- 
-- > img <\\> color == strip color img
(<\\>):: Image -> Color -> Image
(<\\>) img color = strip color img

-- -----------------------------------------------------------------------------
-- Color Filtering

filter_img :: Color -> Image -> Image
filter_img Red   img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x /= 0) i]
filter_img Green img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x /= 1) i]
filter_img Blue  img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x /= 2) i]
filter_img Alpha img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x /= 3) i]

-- |An alias of filter_img
-- 
-- > img <#> color == filter_img color img
(<#>) :: Image -> Color -> Image
(<#>) img color = filter_img color img

-- -----------------------------------------------------------------------------
-- Color Editing 

-- |Edits the value of all colors by multiplying the values by n.
-- 
-- > n > 1 increases brightness
-- > n < 1 decreases brightness
edit_brightness :: Double -> Image -> Image
edit_brightness 1 img = img
edit_brightness n img = img//[(i,edit_brightness' n (img!i)) | i <- indices img]
  where
      edit_brightness' n m
          | floor (n * fromIntegral m) > 255 = 255
          | otherwise = floor $ n * fromIntegral m

-- |Edits the value of the given primary color by multiplying the values by n.
-- 
-- > edit_color Red   0 img == strip Red
-- > edit_color Green 0 img == strip Green
-- > edit_color Blue  0 img == strip Blue
-- > edit_color _     1 img == img
edit_color :: Color -> Double -> Image -> Image
edit_color _ 1 img = img
edit_color Red   n img = img//[(i,edit_red n (img!i)) | i <- indices img,
                               (\(_,_,x) -> x == 0) i]
  where
      edit_red n m
          | floor (n * fromIntegral m) > 255 = 255
          | otherwise = floor $ n * fromIntegral m
edit_color Green n img = img//[(i,edit_green n (img!i)) | i <- indices img,
                               (\(_,_,x) -> x == 1) i]
  where
      edit_green n m
          | floor (n * fromIntegral m) > 255 = 255
          | otherwise = floor $ n * fromIntegral m
edit_color Blue  n img = img//[(i,edit_blue n (img!i)) | i <- indices img,
                               (\(_,_,x) -> x == 2) i]
  where
      edit_blue n m
          | floor (n * fromIntegral m) > 255 = 255
          | otherwise = floor $ n * fromIntegral m
edit_color Alpha n img = img//[(i,edit_alpha n (img!i)) | i <- indices img,
                               (\(_,_,x) -> x == 3) i]
  where
      edit_alpha n m
          | floor (n * fromIntegral m) > 255 = 255
          | otherwise = floor $ n * fromIntegral m

-- |Inverts the colors of the Image.
-- 
-- @ invert_colors == invert_color Red $ invert_color Green 
--                  $ invert_color Blue img @
invert_colors :: Image -> Image
invert_colors img = amap (255-) img

-- |Inverts the values of the color in the image.
-- 
-- @ invert_colors == invert_color Red $ invert_color Green 
--                  $ invert_color Blue img @
invert_color :: Color -> Image -> Image
invert_color Red   img = img//[(i,255-img!i) | i <- indices img, 
                                                    (\(_,_,c) -> c) i == 0]
invert_color Green img = img//[(i,255-img!i) | i <- indices img, 
                                                    (\(_,_,c) -> c) i == 1]
invert_color Blue  img = img//[(i,255-img!i) | i <- indices img, 
                                                    (\(_,_,c) -> c) i == 2]
invert_color Alpha img = img//[(i,255-img!i) | i <- indices img, 
                                                    (\(_,_,c) -> c) i == 3]

-- -----------------------------------------------------------------------------
-- Orientation

-- |Flips the image vertically.
flip_vert :: Image -> Image
flip_vert img = let m = (\(r,_,_) -> r) $ (snd . bounds) img
                in ixmap (bounds img) (\(r,c,v) -> (m-r,c,v)) img

-- |Flips the image horizontally.
flip_horz :: Image -> Image
flip_horz img = let m = (\(_,c,_) -> c) $ (snd . bounds) img
                in ixmap (bounds img) (\(r,c,v) -> (r,m-c,v)) img

-- -----------------------------------------------------------------------------
-- Other --

-- |Merges two images by adding the values contained at each index.
-- Sums over 255 overflow back down to >0.
merge :: Image -> Image -> Image
merge img_1 img_2 = img_1//[(i,img_1!i + img_2!i) | i <- indices img_2]


