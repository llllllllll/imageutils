-----------------------------------------------------------------------------
-- |
-- Module      :  ImageUtils
-- Copyright   :  Joe Jevnik 23.9.2013
-- License     :  GPL v2
-- 
-- Maintainer  :  Joe Jevnik
-- Stability   :  experimental
-- Portability :  requires devIL
--
-- Operations on image files.
--
-----------------------------------------------------------------------------

module ImageUtils 
    ( module Data.Array.Unboxed
    , module Codec.Image.DevIL
    , PrimaryColor(..)
    , Color
    , Image
    -- * Colors
    , rED
    , gREEN
    , bLUE
    , cYAN
    , bLACK
    , wHITE
    -- * Primary color changes
    , strip           -- :: PrimaryColor -> Image -> Image
    , (<\\>)          -- :: Image -> PrimaryColor -> Image
    , filter_img      -- :: PrimaryColor -> Image -> Image
    , (<#>)           -- :: Image -> PrimaryColor -> Image
    , edit_brightness -- :: Double -> Image
    , edit_primary      -- :: PrimaryColor -> Double -> Image
    , invert_primarys   -- :: Image -> Image
    , invert_primary    -- :: PrimaryColor -> Image
    -- * Orientation
    , flip_vert       -- :: Image -> Image
    , flip_horz       -- :: Image -> Image
    -- * Drawing
    , draw_seg        -- :: (Int,Int) -> (Int,Int) -> Color -> Image -> Image
    , draw_ln         -- :: (Int,Int) -> (Int,Int) -> Color -> Image -> Image
    -- * Other
    , merge           -- :: Image -> Image -> Image
    ) where

import Codec.Image.DevIL
import Data.Array.Unboxed

-- |Data type for primary colors
data PrimaryColor = Red | Green | Blue

-- |The color channels on in the RGB images.
type Color = (Word8,Word8,Word8)

-- |Alias for the type of data returned from readImage.
type Image = UArray (Int,Int,Int) Word8 -- Indices: (R,G,B)

-- |Red color triplet.
rED   :: Color
rED   = (255,0,0)

-- |Green color triplet.
gREEN :: Color
gREEN = (0,255,0)

-- |Blue color triplet.
bLUE  :: Color
bLUE  = (0,0,255)

-- |Cyan color triplet.
cYAN :: Color
cYAN = (0,255,255)

-- |Black color triplet.
bLACK :: Color
bLACK = (0,0,0)

-- |White color triplet.
wHITE :: Color
wHITE = (255,255,255)


-- Testing functions.
main :: IO ()
main = do
    ilInit
    let img = listArray ((0,0,0),(499,499,3)) (repeat 255)
    writeImage "../testln2.jpg" (draw_seg (24,40) (50,25) (0,0,0) img)

-- -----------------------------------------------------------------------------
-- PrimaryColor stripping

-- |Strips an Image of all of a primary color.

strip :: PrimaryColor -> Image -> Image
strip Red   img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x == 0) i]
strip Green img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x == 1) i]
strip Blue  img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x == 2) i]

-- |An alias of strip.
-- 
-- > img <\\> color == strip color img
(<\\>):: Image -> PrimaryColor -> Image
(<\\>) img color = strip color img

-- -----------------------------------------------------------------------------
-- PrimaryColor Filtering

filter_img :: PrimaryColor -> Image -> Image
filter_img Red   img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x /= 0) i]
filter_img Green img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x /= 1) i]
filter_img Blue  img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x /= 2) i]

-- |An alias of filter_img
-- 
-- > img <#> color == filter_img color img
(<#>) :: Image -> PrimaryColor -> Image
(<#>) img color = filter_img color img

-- -----------------------------------------------------------------------------
-- PrimaryColor Editing 

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
-- > edit_primary Red   0 img == strip Red
-- > edit_primary Green 0 img == strip Green
-- > edit_primary Blue  0 img == strip Blue
-- > edit_primary _     1 img == img
edit_primary :: PrimaryColor -> Double -> Image -> Image
edit_primary _ 1 img = img
edit_primary Red   n img = img//[(i,edit_red n (img!i)) | i <- indices img,
                                 (\(_,_,x) -> x == 0) i]
  where
      edit_red n m
          | floor (n * fromIntegral m) > 255 = 255
          | otherwise = floor $ n * fromIntegral m
edit_primary Green n img = img//[(i,edit_green n (img!i)) | i <- indices img,
                                 (\(_,_,x) -> x == 1) i]
  where
      edit_green n m
          | floor (n * fromIntegral m) > 255 = 255
          | otherwise = floor $ n * fromIntegral m
edit_primary Blue  n img = img//[(i,edit_blue n (img!i)) | i <- indices img,
                                 (\(_,_,x) -> x == 2) i]
  where
      edit_blue n m
          | floor (n * fromIntegral m) > 255 = 255
          | otherwise = floor $ n * fromIntegral m

-- |Inverts the colors of the Image.
-- 
-- @ invert_primarys == invert_primary Red $ invert_primary Green 
--                  $ invert_primary Blue img @
invert_primarys :: Image -> Image
invert_primarys img = amap (255-) img

-- |Inverts the values of the color in the image.
-- 
-- @ invert_primarys == invert_primary Red $ invert_primary Green 
--                  $ invert_primary Blue img @
invert_primary :: PrimaryColor -> Image -> Image
invert_primary Red   img = img//[(i,255-img!i) | i <- indices img, 
                                                      (\(_,_,c) -> c) i == 0]
invert_primary Green img = img//[(i,255-img!i) | i <- indices img, 
                                                      (\(_,_,c) -> c) i == 1]
invert_primary Blue  img = img//[(i,255-img!i) | i <- indices img, 
                                                      (\(_,_,c) -> c) i == 2]
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
-- Drawing

-- |Draws a straight line segment from (x,y) to (x',y') in color c
draw_seg :: (Int,Int) -> (Int,Int) -> Color -> Image -> Image
draw_seg (x,y) (x',y') c img 
    | x == x'   = img//[((m,x,v),val v c) | m <- [min y y'..max y y']
                       , v <- [0..2]]
    | otherwise = let sl = fromIntegral (y' - y) / fromIntegral (x' - x)
                  in img//[((m,n,v),val v c) | n <- [min x x'..max x x']
                          , let m = round (sl * fromIntegral (n - x)) + y
                          , v <- [0..2]]
  where
      val 0 (r,_,_) = r
      val 1 (_,g,_) = g
      val 2 (_,_,b) = b

-- |Draws a line that intersects (x,y) and (x',y') in color c
-- MUST FIX FOR NON HORIZONTAL OR VERTICAL!
draw_ln :: (Int,Int) -> (Int,Int) -> Color -> Image -> Image
draw_ln (x,y) (x',y') c img
    | x == x'   = img//[((m,x,v),val v c) | m <- [0..(\(r,_,_) -> r) 
                                                         ((snd . bounds) img)]
                       , v <- [0..2]]
    | otherwise = let sl    = fromIntegral (y' - y) / fromIntegral (x' - x)
                      max_x = (\(_,c,_) -> c) ((snd . bounds) img)
                  in img//[((m,n,v),val v c) | n <- [0..max_x]
                          , let m = round (sl * fromIntegral (n - x)) + y
                          , v <- [0..2]]
  where
      val 0 (r,_,_) = r
      val 1 (_,g,_) = g
      val 2 (_,_,b) = b

-- -----------------------------------------------------------------------------
-- Other --

-- |Merges two images by adding the values contained at each index.
-- Sums over 255 overflow back down to >0.
merge :: Image -> Image -> Image
merge img_1 img_2 = img_1//[(i,img_1!i + img_2!i) | i <- indices img_2]


