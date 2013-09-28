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
    -- * Properties
    , width           -- :: Image -> Int
    , height          -- :: Image -> Height
    -- * Colors
    , rED
    , gREEN
    , bLUE
    , cYAN
    , bLACK
    , wHITE
    -- * Primary color changes
    , strip           -- :: PrimaryColor -> Image -> Image
    , (<//>)          -- :: Image -> PrimaryColor -> Image
    , filter_img      -- :: PrimaryColor -> Image -> Image
    , (<#>)           -- :: Image -> PrimaryColor -> Image
    , edit_brightness -- :: Double -> Image
    , edit_primary    -- :: PrimaryColor -> Double -> Image
    , invert_primarys -- :: Image -> Image
    , invert_primary  -- :: PrimaryColor -> Image
    -- * Orientation
    , flip_vert       -- :: Image -> Image
    , flip_horz       -- :: Image -> Image
    -- * Drawing
    , draw_seg        -- :: (Int,Int) -> (Int,Int) -> Color -> Image -> Image
    , draw_ln         -- :: (Int,Int) -> (Int,Int) -> Color -> Image -> Image
    -- * Composing
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

-- Testing functions.
gen_test_cases :: IO ()
gen_test_cases = do
    ilInit
    img <- readImage "../img/ducks.jpg"
    writeImage "../img/test_str_r.jpg" (img  <//> Red)
    writeImage "../img/test_str_g.jpg" (img  <//> Green)
    writeImage "../img/test_str_b.jpg" (img  <//> Blue)
    writeImage "../img/test_fil_r.jpg" (img  <#>  Red)
    writeImage "../img/test_fil_g.jpg" (img  <#>  Green)
    writeImage "../img/test_fil_b.jpg" (img  <#>  Blue)
    writeImage "../img/test_flp_h.jpg" (flip_horz img)
    writeImage "../img/test_flp_v.jpg" (flip_vert img)
    writeImage "../img/test_til_h.jpg" (img >++> (flip_horz img))
    writeImage "../img/test_til_v.jpg" (img ^++^ (flip_vert img))

pop_art :: IO ()
pop_art = do
    ilInit
    img <- readImage "../img/ducks.jpg"
    writeImage "../img/pop_art.jpg" (((img <//> Red) ^++^ (img <//> Green)) >++>
                                     ((img <//> Blue) ^++^ img))
    
-- -----------------------------------------------------------------------------
-- Properties

-- |Returns the height of img in pixels
height :: Image -> Int
height img = ((\(r,_,_) -> r) $ (snd . bounds) img) + 1

-- |Returns the width of img in pixels.
width :: Image -> Int
width img = ((\(_,c,_) -> c) $ (snd . bounds) img) + 1

d_indices :: Image -> [(Int,Int)]
d_indices img = zip [1..height img - 1] [0..width img - 1]

-- -----------------------------------------------------------------------------
-- Colors

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

-- -----------------------------------------------------------------------------
-- PrimaryColor stripping

-- |Strips an Image of all of a primary color.

strip :: PrimaryColor -> Image -> Image
strip Red   img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x == 0) i]
strip Green img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x == 1) i]
strip Blue  img = img//[(i,0) | i <- indices img, (\(_,_,x) -> x == 2) i]

-- |An alias of strip.
-- 
-- > img <//> color == strip color img
(<//>):: Image -> PrimaryColor -> Image
(<//>) img color = strip color img

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
edit_brightness n img = img//[(i,floor (n * fromIntegral (img!i))) 
                                  | i <- indices img]


-- |Edits the value of the given primary color by multiplying the values by n.
-- 
-- > edit_primary Red   0 img == strip Red
-- > edit_primary Green 0 img == strip Green
-- > edit_primary Blue  0 img == strip Blue
-- > edit_primary _     1 img == img
edit_primary :: PrimaryColor -> Double -> Image -> Image
edit_primary _ 1 img = img
edit_primary Red   n img = img//[(i,floor (n * fromIntegral (img!i))) 
                                     | (r,c) <- d_indices img, let i = (r,c,0)]
edit_primary Green n img = img//[(i,floor (n * fromIntegral (img!i))) 
                                     | (r,c) <- d_indices img, let i = (r,c,1)]
edit_primary Blue  n img = img//[(i,floor (n * fromIntegral (img!i))) 
                                     | (r,c) <- d_indices img, let i = (r,c,2)]

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
invert_primary Red   img = img//[(i,255 - img!i) | (r,c) <- d_indices img
                                , let i = (r,c,0)]
invert_primary Green img = img//[(i,255 - img!i) | (r,c) <- d_indices img
                                , let i = (r,c,1)]
invert_primary Blue  img = img//[(i,255 - img!i) | (r,c) <- d_indices img
                                , let i = (r,c,2)]

-- -----------------------------------------------------------------------------
-- Orientation

-- |Flips the image vertically.
flip_vert :: Image -> Image
flip_vert img = let m = height img - 1
                in ixmap (bounds img) (\(r,c,v) -> (m-r,c,v)) img

-- |Flips the image horizontally.
flip_horz :: Image -> Image
flip_horz img = let m = width img - 1
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
    | x == x'   = img//[((m,x,v),val v c) | m <- [0..height img], v <- [0..2]]
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
-- Composing

-- |Merges two images by adding the values contained at each index.
-- Sums over 255 overflow back down to >0.
merge :: Image -> Image -> Image
merge img_1 img_2 = img_1//[(i,img_1!i + img_2!i) | i <- indices img_2]

-- |Alias for merge.
(>++<) :: Image -> Image -> Image
(>++<) img_1 img_2 = merge img_1 img_2


-- |Tiles the two images horizontally. The dimensions are the sum of the widths
-- by the greater of the two heights. If the images are not the same size, black
-- will fill in around the gaps.
tile_horz :: Image -> Image -> Image
tile_horz l_img r_img = let h    = max (height l_img - 1) (height r_img - 1)
                            w    = width l_img + width r_img - 1
                            w_os = width l_img
                        in (listArray ((0,0,0),(h,w,3)) (repeat 0)//
                            [(i,l_img!i) | i <- indices l_img])//
                               [(i,r_img!i') | i'@(r,c,v) <- indices r_img
                               , let i = (r,c+w_os,v)]
                                                  
-- |Alias for tile_horz.
(>++>) :: Image -> Image -> Image
(>++>) l_img r_img = tile_horz l_img r_img


-- |Tiles two images vertically. The dimensions will be the greater of the
-- widths and the sum of their heights. If the images are not the same size,
-- black will fill around the gaps.
tile_vert :: Image -> Image -> Image
tile_vert l_img u_img = let h    = height l_img + height u_img - 1
                            w    = max (width l_img) (width u_img)
                            h_os = height l_img
                        in (listArray ((0,0,0),(h,w,3)) (repeat 0)//
                            [(i,l_img!i) | i <- indices l_img])//
                               [(i,u_img!i') | i'@(r,c,v) <- indices u_img
                               , let i = (r+h_os,c,v)]

-- |An alias for tile_vert.
(^++^) l_img u_img = tile_vert l_img u_img
