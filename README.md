imageutils
===========

Utilities for working with images in haskell

data and type synonyms:
-----------------------------


<pre>

    , PrimaryColor(..)
    , Color
    , Image

</pre>


Image Properties:
-----------------

<pre>

    , width            -- :: Image -> Int
    , height           -- :: Image -> Height
    , d_indices        -- :: Image -> [(Int,Int)]

</pre>


Colors:
-------


<pre>

    , rED              -- :: Color
    , gREEN            -- :: Color
    , bLUE             -- :: Color
    , cYAN             -- :: Color
    , bLACK            -- :: Color
    , wHITE            -- :: Color

</pre>


Primary Color Changes:
----------------------

<pre>

    , strip            -- :: PrimaryColor -> Image -> Image
    , (<//>)           -- :: Image -> PrimaryColor -> Image
    , filter_img       -- :: PrimaryColor -> Image -> Image
    , (<##>)           -- :: Image -> PrimaryColor -> Image
    , edit_brightness  -- :: Double -> Image
    , edit_primary     -- :: PrimaryColor -> Double -> Image
    , invert_primaries -- :: Image -> Image
    , invert_primary   -- :: PrimaryColor -> Image

</pre>


Color Tools:
------------

<pre>

    , black_and_white  -- :: Double -> Image -> Image
    , gs_light         -- :: Image -> Image
    , gs_avg           -- :: Image -> Image
    , gs_lum           -- :: Image -> Image

</pre>


Orientation:
------------

<pre>

    , flip_vert        -- :: Image -> Image
    , flip_horz        -- :: Image -> Image

</pre>


Drawing:
--------

<pre>

    , draw_seg         -- :: (Int,Int) -> (Int,Int) -> Color -> Image -> Image
    , draw_ln          -- :: (Int,Int) -> (Int,Int) -> Color -> Image -> Image

</pre>


Composing:
----------

<pre>

    , merge            -- :: Image -> Image -> Image
    , (>++<)           -- :: Image -> Image -> Image
    , tile_horz        -- :: Image -> Image -> Image
    , (>++>)           -- :: Image -> Image -> Image
    , tile_vert        -- :: Image -> Image -> Image
    , (^++^)           -- :: Image -> Image -> Image

</pre>


Todo:
-----

- Fix draw_ln
- Add more functions
