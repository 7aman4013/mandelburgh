{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognized-pragmas #-}
module Main where

import Codec.Picture 
import Data.Complex (Complex((:+)))
import Data.Word (Word8)
import Data.Fixed (mod')
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Text.Printf (printf)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Parallel.Strategies 
import Control.DeepSeq (NFData(..), force)
import Data.Time (getCurrentTime, diffUTCTime)
import GHC.Conc (numCapabilities)
import Data.Data (Data)

-- NFData instance for PixelRGB8
instance NFData PixelRGB8 where
    rnf (PixelRGB8 r g b) = r `seq` g `seq` b `seq` ()

---- Constants moved to top level
maxIter :: Int
maxIter = 10000

bailoutRadius :: Double
bailoutRadius = sqrt 8 -- magnitude squared

juliaC :: Complex Double
juliaC = (-0.9) :+ 0.4  -- This gives a nice looking Julia set

defaultWidth, defaultHeight :: Int
defaultWidth  = 10000
defaultHeight = 10000

defaultZoom, defaultCenterX, defaultCenterY :: Double
defaultZoom   = 16e-3 -- Bigger values = less zoom
defaultCenterX = -0.7453 ---0.743643887037151 
defaultCenterY = 0.1127 --0.131825904205330

-- 3.0, -0.75, 0.0 for mandelbrot
-- 1e-[1..5], -0.743643887037151, 0.131825904205330 are also nice spots
-- 16e-3, -0.7453, 0.1127 as well


---- "juliabruh.png" settings: 
-- (0.5, 1, 0), (c = -0.9 + 0.4i), (rad^2 = 3.0), (1000 iterations)

-- Vector-based color palette using tuples (unboxable)
{-# INLINE colorPalette #-}
colorPalette :: V.Vector (Word8, Word8, Word8)
colorPalette = V.generate 360 $ \i ->
    let h = fnt i
    in hsvToRgb (h, 1.0, 1.0)

-- Color mapping function using smooth coloring
-- (Literal black magic to me now)
{-# INLINE colorMapping #-}
colorMapping :: (Int, Double) -> PixelRGB8
colorMapping (iter, smooth)
    | iter == maxIter = PixelRGB8 0 0 0
    | otherwise =
        let hue = floor ((smooth * 20) `mod'` 360)
            intensity = 1.0 - sqrt (fnt iter / fnt maxIter)
            (r, g, b) = colorPalette V.! hue
            scale x = round (fnt x * intensity)
        in PixelRGB8 (scale r) (scale g) (scale b)

-- (same with this)
{-# INLINE hsvToRgb #-}
hsvToRgb :: (Double, Double, Double) -> (Word8, Word8, Word8)
hsvToRgb (h, s, v) = (round (255 * r), round (255 * g), round (255 * b))
  where
    c = v * s
    h' = h / 60
    x = c * (1 - abs ((h' `mod'` 2) - 1))
    m = v - c
    (r1, g1, b1)
        | h' < 1 = (c, x, 0)
        | h' < 2 = (x, c, 0)
        | h' < 3 = (0, c, x)
        | h' < 4 = (0, x, c)
        | h' < 5 = (x, 0, c)
        | otherwise = (c, 0, x)
    r = r1 + m
    g = g1 + m
    b = b1 + m

-- I'm lazy and this clutters everything
{-# INLINE fnt #-}
fnt :: (Integral a, Num b) => a -> b
fnt = fromIntegral

{-# INLINE magnitudeSquared #-}
magnitudeSquared :: Complex Double -> Double
magnitudeSquared (r :+ i) = r*r + i*i

{-# INLINE smoothValue #-}
smoothValue :: Double -> Double
smoothValue mag = (fnt maxIter) + 1 - logBase 4 (log mag)

-- Escape-time calculation using period checking and early bailout
{-# INLINE mandelbrotSmooth #-}
mandelbrotSmooth :: Double -> Double -> (Int, Double)
mandelbrotSmooth !x !y = go 0 (0 :+ 0) (0 :+ 0) 0
  where
    c = x :+ y

    go !period !z !oldZ !iter
        | iter >= maxIter = (maxIter, 0)
        | magnitude2 > bailoutRadius = (iter, smoothValue magnitude2)
        | period > 20 && (magnitudeSquared (z - oldZ) < 1e-6) = (maxIter, 0)
        | otherwise = go period' z' checkZ (iter + 1)
      where
        ---- Actual mandelbrot stuff
        !z' = z*z + c
        !magnitude2 = magnitudeSquared z
        !period' = if period >= 20 then 1 else period + 1
        !checkZ = if period' == 20 then z' else oldZ

-- Julia
{-# INLINE juliaSmooth #-}
juliaSmooth :: Double -> Double -> (Int, Double)
juliaSmooth !x !y = go 0 (x :+ y) (0 :+ 0) 0
  where
    go !period !z !oldZ !iter
        | iter >= maxIter = (maxIter, 0)
        | magnitude2 > bailoutRadius = (iter, smoothValue magnitude2)
        | period > 20 && (magnitudeSquared (z - oldZ) < 1e-6) = (maxIter, 0)
        | otherwise = go period' z' checkZ (iter + 1)
      where
        -- Actual julia stuff
        !z' = z*z + juliaC  -- fixed constant
        !magnitude2 = magnitudeSquared z
        !period' = if period >= 20 then 1 else period + 1
        !checkZ = if period' == 20 then z' else oldZ

type FractalFunc = Double -> Double -> (Int, Double)
type FractalGenerator = Int -> Int -> Double -> Double -> Double -> Double -> Image PixelRGB8
type PixelRow = V.Vector (Int, PixelRGB8)

-- Generic row generator with strict evaluation and better memory usage
generateRow :: FractalFunc -> Int -> Int -> Int -> Double -> Double -> Double -> Double -> PixelRow
generateRow fractalFunc !width !y !height !centerX !centerY !zoom !aspectRatio =
    V.generate width $ \x -> 
        let !sx = centerX + (fnt x / fnt width - 0.5) * zoom * aspectRatio
            !sy = centerY + (fnt y / fnt height - 0.5) * zoom
            !result = fractalFunc sx sy
        in (x, colorMapping result)

-- Generic fractal generator with optimized chunking
generateFractal :: FractalFunc -> Int -> Int -> Double -> Double -> Double -> Double -> Image PixelRGB8
generateFractal fractalFunc !width !height !centerX !centerY !zoom !aspectRatio =
    generateImage pixelRenderer width height
  where
    -- Larger chunks for better parallelization
    blockSize = max 128 (height `div` (8 * numCapabilities))
    numBlocks = (height + blockSize - 1) `div` blockSize
    
    !rows = V.concat $ force $ parMap rpar
        (\block -> 
            let !start = block * blockSize
                !end = min (start + blockSize) height
            in V.fromList
                [generateRow fractalFunc width y height centerX centerY zoom aspectRatio
                 | y <- [start..end-1]]
        ) [0..numBlocks-1]

    pixelRenderer x y = snd (rows V.! y V.! x)

-- Specific generators using the generic one
generateM :: FractalGenerator
generateM = generateFractal mandelbrotSmooth

generateJ :: FractalGenerator
generateJ = generateFractal juliaSmooth

main :: IO ()
main = do
    args <- getArgs
    {-# HLINT ignore "Use fromMaybe" #-}
    let (width, height, fractalType) = case args of
            (w:h:t:_) -> (maybe defaultWidth id (readMaybe w),
                         maybe defaultHeight id (readMaybe h),
                         t)
            (w:h:_)   -> (maybe defaultWidth id (readMaybe w),
                         maybe defaultHeight id (readMaybe h),
                         "mandelbrot")
            _         -> (defaultWidth, defaultHeight, "mandelbrot")

    let aspectRatio = fnt width / fnt height
        (generator, filename) = case fractalType of
            "julia" -> (generateJ, "juliabruh.png")
            _       -> (generateM, "mandelbruh.png")

    printf "Generating %dx%d %s set...\n" width height fractalType

    startTime <- getCurrentTime

    let !img = generator
            width
            height
            defaultCenterX
            defaultCenterY
            defaultZoom
            aspectRatio

    endTime <- getCurrentTime
    let timeTaken = diffUTCTime endTime startTime

    printf "Generated in %s seconds!\n" (show timeTaken)
    savePngImage filename (ImageRGB8 img)
    printf "Image saved as '%s'\n" filename
