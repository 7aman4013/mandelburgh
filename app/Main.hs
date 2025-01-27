{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fromMaybe" #-}
module Main where

import Codec.Picture
import Data.Complex
import Data.Word
import Data.Fixed
import Control.Parallel.Strategies
import System.Environment (getArgs)
import Text.Read (readMaybe)
import qualified Data.Vector as V
import Control.Parallel.Strategies (parMap, rpar)
import Control.DeepSeq (NFData(..), force)
import Data.Time (getCurrentTime, diffUTCTime)

-- NFData instance for PixelRGB8
instance NFData PixelRGB8 where
    rnf (PixelRGB8 r g b) = r `seq` g `seq` b `seq` ()

---- Constants moved to top level
maxIter :: Int
maxIter = 1000

bailoutRadius :: Double
bailoutRadius = 4.0 -- magnitude squared

juliaC :: Complex Double
juliaC = (-0.2) :+ 0.8  -- This gives a nice looking Julia set

defaultWidth, defaultHeight :: Int
defaultWidth  = 1000
defaultHeight = 1000

defaultZoom, defaultCenterX, defaultCenterY :: Double
defaultZoom   = 0.01 -- Bigger values = less zoom
defaultCenterX = -0.743643887037151 
defaultCenterY = 0.131825904205330

---- 3.0, -0.75, 0.0 for mandelbrot

-- Vector-based color palette using tuples (which are Unbox-able)
{-# INLINE colorPalette #-}
colorPalette :: V.Vector (Word8, Word8, Word8)
colorPalette = V.generate 360 $ \i ->
    let h = fnt i
    in hsvToRgb (h, 1.0, 1.0)

-- Optimized color mapping using palette lookup
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

{-# INLINE hsvToRgb #-}
hsvToRgb :: (Double, Double, Double) -> (Word8, Word8, Word8)
hsvToRgb (!h, !s, !v) = (round (255 * r), round (255 * g), round (255 * b))
  where
    !c = v * s
    !h' = h / 60
    !x = c * (1 - abs ((h' `mod'` 2) - 1))
    !m = v - c
    (!r1, !g1, !b1)
        | h' < 1 = (c, x, 0)
        | h' < 2 = (x, c, 0)
        | h' < 3 = (0, c, x)
        | h' < 4 = (0, x, c)
        | h' < 5 = (x, 0, c)
        | otherwise = (c, 0, x)
    !r = r1 + m
    !g = g1 + m
    !b = b1 + m

type PixelRow = V.Vector (Int, PixelRGB8)

-- Strict helper for non-integer conversion
{-# INLINE fnt #-}
fnt :: (Integral a, Num b) => a -> b
fnt = fromIntegral

-- Optimized escape-time calculation using period checking and early bailout
{-# INLINE mandelbrotSmooth #-}
mandelbrotSmooth :: Double -> Double -> (Int, Double)
mandelbrotSmooth !x !y = go 0 (0 :+ 0) (0 :+ 0) 0
  where
    c = x :+ y

    go !period !z !oldZ !iter
        | iter >= maxIter = (maxIter, 0)
        | magnitude2 > bailoutRadius = (iter, smoothValue magnitude2)
        | period > 20 && z == oldZ = (maxIter, 0)  -- Period detection
        | otherwise = go period' z' checkZ (iter + 1)
      where
        ---- Main function
        !z' = z*z + c
        !magnitude2 = magnitudeSquared z
        !period' = if period >= 20 then 1 else period + 1
        !checkZ = if period' == 20 then z' else oldZ

    -- Optimized magnitude calculation avoiding sqrt
    magnitudeSquared :: Complex Double -> Double
    magnitudeSquared (r :+ i) = r*r + i*i

    -- Optimized smooth value calculation
    smoothValue mag = iter + 1 - logBase 4 (log mag)
        where iter = fnt maxIter

generateRowM :: Int -> Int -> Int -> Double -> Double -> Double -> Double -> PixelRow
generateRowM width y height centerX centerY zoom aspectRatio =
    V.generate width (\x -> (x, colorMapping $ mandelbrotSmooth (scaledX x) (scaledY y)))
  where
    scaledX x = centerX + (fnt x / fnt width - 0.5) * zoom * aspectRatio
    scaledY y = centerY + (fnt y / fnt height - 0.5) * zoom

generateM :: Int -> Int -> Double -> Double -> Double -> Double -> Image PixelRGB8
generateM width height centerX centerY zoom aspectRatio =
    generateImage pixelRenderer width height
  where
    -- Generate rows in parallel in blocks
    blockSize = 100 -- Blocks didnt seem to help very much unless massive
    !rowBlocks = force $ parMap rpar
        (\block ->
            [generateRowM width y height centerX centerY zoom aspectRatio
             | y <- [block * blockSize..min ((block + 1) * blockSize - 1) (height - 1)]]
        ) [0..(height `div` blockSize)]

    -- Flatten the blocks into a single array of rows
    !allRows = concat rowBlocks

    -- Lookup function for pixel renderer
    pixelRenderer x y =
        let row = allRows !! y
            (_, pixel) = row V.! x
        in pixel


-- Optimized escape-time calculation for Julia set
{-# INLINE juliaSmooth #-}
juliaSmooth :: Double -> Double -> (Int, Double)
juliaSmooth !x !y = go 0 (x :+ y) (0 :+ 0) 0
  where
    go !period !z !oldZ !iter
        | iter >= maxIter = (maxIter, 0)
        | magnitude2 > bailoutRadius = (iter, smoothValue magnitude2)
        | period > 20 && z == oldZ = (maxIter, 0)  -- Period detection
        | otherwise = go period' z' checkZ (iter + 1)
      where
        -- Main function (Julia set iteration)
        !z' = z*z + juliaC **2 - juliaC  -- Using fixed juliaC instead of pixel coordinates
        !magnitude2 = magnitudeSquared z
        !period' = if period >= 20 then 1 else period + 1
        !checkZ = if period' == 20 then z' else oldZ

    -- Optimized magnitude calculation avoiding sqrt
    magnitudeSquared :: Complex Double -> Double
    magnitudeSquared (r :+ i) = r*r + i*i

    -- Optimized smooth value calculation
    smoothValue mag = iter + 1 - logBase 4 (log mag)
        where iter = fnt maxIter

generateRowJ :: Int -> Int -> Int -> Double -> Double -> Double -> Double -> PixelRow
generateRowJ width y height centerX centerY zoom aspectRatio =
    V.generate width (\x -> (x, colorMapping $ juliaSmooth (scaledX x) (scaledY y)))
  where
    scaledX x = centerX + (fnt x / fnt width - 0.5) * zoom * aspectRatio
    scaledY y = centerY + (fnt y / fnt height - 0.5) * zoom

generateJ :: Int -> Int -> Double -> Double -> Double -> Double -> Image PixelRGB8
generateJ width height centerX centerY zoom aspectRatio =
    generateImage pixelRenderer width height
  where
    -- Generate rows in parallel in blocks
    blockSize = 1000 -- Blocks didnt seem to help very much unless massive
    !rowBlocks = force $ parMap rpar
        (\block ->
            [generateRowJ width y height centerX centerY zoom aspectRatio
             | y <- [block * blockSize..min ((block + 1) * blockSize - 1) (height - 1)]]
        ) [0..(height `div` blockSize)]

    -- Flatten the blocks into a single array of rows
    !allRows = concat rowBlocks

    -- Lookup function for pixel renderer
    pixelRenderer x y =
        let row = allRows !! y
            (_, pixel) = row V.! x
        in pixel

-- Main function with parallel processing
main :: IO ()
main = do
    args <- getArgs
    let (width, height) = case args of
            (w:h:_) -> (maybe defaultWidth id (readMaybe w),
                       maybe defaultHeight id (readMaybe h))
            _ -> (defaultWidth, defaultHeight)

    let aspectRatio = fnt width / fnt height
    putStrLn $ "Generating " ++ show width ++ "x" ++ show height ++ " Mandelbrot set..."

    startTime <- getCurrentTime

    -- Generate image with parallel processing
    let !img = generateM
            width
            height
            defaultCenterX
            defaultCenterY
            defaultZoom
            aspectRatio

    endTime <- getCurrentTime
    let timeTaken = diffUTCTime endTime startTime

    putStrLn $ "Generated in " ++ show timeTaken ++ " seconds!"
    savePngImage "mandelbruh.png" (ImageRGB8 img)
    putStrLn "Image saved as 'mandelbruh.png'"
