module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Complex
import Control.Parallel.Strategies (parMap, rpar)

{----- SCROLL DOWN TO THE BOTTOM FOR CUSTOMIZATION! ------}


-- Convert integral types to floating-point values.
fnt :: (Integral a, Num b) => a -> b
fnt = fromIntegral

-- Generalized Mandelbrot function
generalMandelbrot :: (Complex Double  -> Complex Double  -> Double  -> Complex Double ) -> Complex Double  -> Int -> Int
generalMandelbrot f c n = go 0 0
  where
    go z count
      | count >= n    = n
      | magnitude z > maximumMagnitude = count
      | otherwise     = go (f z c 0) (count + 1)

-- Function with time-dependent scaling on `c`.
generalMandelbrotTimed :: (Complex Double  -> Complex Double  -> Double  -> Complex Double ) -> Complex Double  -> Int -> Double  -> Int
generalMandelbrotTimed f c n t = go 0 0
  where
    go z count
      | count >= n    = n
      | magnitude z > maximumMagnitude = count
      | otherwise     = go (f z c t) (count + 1)

-- Translates the number of iterations into a color value for a point
colorMapping :: Int -> Int -> Color
colorMapping i n
  | i == n    = black
  | otherwise = makeColorI (i * 255 `div` n) 0 (255 - i * 255 `div` n) 255

-- Render the function for a given pixel, with and without time dependency
generalMandelbrotColor :: (Complex Double  -> Complex Double  -> Double  -> Complex Double ) -> Double  -> Double  -> Int -> Color
generalMandelbrotColor f x y n = colorMapping (generalMandelbrot f (x :+ y) n) n

generalMandelbrotColorTimed :: (Complex Double  -> Complex Double  -> Double  -> Complex Double ) -> Double  -> Double  -> Int -> Double  -> Color
generalMandelbrotColorTimed f x y n t = colorMapping (generalMandelbrotTimed f (x :+ y) n t) n

-- Generates the image for a given size, applying scaling and offsets
mandelbrotImage :: (Complex Double  -> Complex Double  -> Double  -> Complex Double ) -> Int -> Int -> Int -> Double  -> Double  -> Double  -> Double  -> Double  -> Picture
mandelbrotImage f w h n scaleX scaleY offsetX offsetY t =
  Pictures $ parMap rpar createPixel [(x, y) | x <- [0, blockSize .. w], y <- [0, blockSize .. h]]
  where
    createPixel (x, y) = translate (fnt x - fnt w / 2) (fnt y - fnt h / 2) $ color (generalMandelbrotColorTimed f (scaledX x) (scaledY y) n t) (rectangleSolid (fromIntegral blockSize) (fromIntegral blockSize))
    scaledX x = offsetX + (fnt x / fnt w - 0.5) * scaleX
    scaledY y = offsetY + (fnt y / fnt h - 0.5) * scaleY
    blockSize = 1

-- Handle panning with WASD and zooming with Q/E
handleInput :: Event -> (Double , Double , Double , Double ) -> (Double , Double , Double , Double )
handleInput (EventKey (Char 'w') Down _ _) (scaleX, scaleY, offsetX, offsetY) =
    (scaleX, scaleY, offsetX, offsetY + 0.3 * scaleY)  -- Pan up
handleInput (EventKey (Char 'a') Down _ _) (scaleX, scaleY, offsetX, offsetY) =
    (scaleX, scaleY, offsetX - 0.3 * scaleX, offsetY)  -- Pan left
handleInput (EventKey (Char 's') Down _ _) (scaleX, scaleY, offsetX, offsetY) =
    (scaleX, scaleY, offsetX, offsetY - 0.3 * scaleY)  -- Pan down
handleInput (EventKey (Char 'd') Down _ _) (scaleX, scaleY, offsetX, offsetY) =
    (scaleX, scaleY, offsetX + 0.3 * scaleX, offsetY)  -- Pan right

handleInput (EventKey (Char 'q') Down _ _) (scaleX, scaleY, offsetX, offsetY) =
    (scaleX * 2, scaleY * 2, offsetX, offsetY)  -- Zoom out
handleInput (EventKey (Char 'e') Down _ _) (scaleX, scaleY, offsetX, offsetY) =
    (scaleX * 0.5, scaleY * 0.5, offsetX, offsetY)  -- Zoom in

handleInput (EventMotion _) state = state  -- Ignore mouse motion events
handleInput _ state = state  -- Ignore other events

-- Animate function that handles the Mandelbrot set with time, zoom, and panning 
animateMandelbrot :: (Complex Double  -> Complex Double  -> Double  -> Complex Double ) -> Int -> Int -> Int -> IO () 
animateMandelbrot f w h n = do 
  let initialState = (3.0 * fnt screenWidth / fnt screenHeight, 3.0, 0, 0) -- (scaleX, scaleY, offsetX, offsetY) 
  play 
      (InWindow "Mandelbrot Set Animation" (w, h) (100, 100)) 
      white 
      fps
      (initialState, 0) -- (State, Time) 
      (\((scaleX, scaleY, offsetX, offsetY), t) -> mandelbrotImage f w h n scaleX scaleY offsetX offsetY t) 
      (\e ((scaleX, scaleY, offsetX, offsetY), t) -> (handleInput e (scaleX, scaleY, offsetX, offsetY), t)) 
      (\_ (state, t) -> (state, t + 1 / fnt fps)) -- Increment time

main :: IO ()
main = animateMandelbrot (mandelbrotSet) screenWidth screenHeight maximumIterations 
{------- CUSTOMIZATION IS HERE ^^^^^^^ --------}


---- time is included so that we don't have to shuffle through the functions depending on if we include it or not.
mandelbrotSet z c t = z**2 + c
aquariumSet z c t = pi**(c**(-0.5)) + (z**2 + c) ---- Keep zooming in towards the centre! :) 
lobsterSet z c t = exp(-(1+exp(-c))) + (z**2 + c**(-exp(z))) ---- Zoom out!
badguySet z c t = z**(c) + c**(t**(0.5) :+ 0) 
pyramidSet z c t= z**(c**2) - z**(c) + 0*c**(t :+ 0) - c**(t**(-0.5) :+ 1) 
alienSet z c t = (z - c) / (conjugate (z**(t**(-5) :+ 0)) + conjugate c) + conjugate z ---- Zoom out! :D
tsunamiSet z c t = z / exp (c / (t :+ 0)) - cos z ---- Keep zooming out before the waves swallow you!

maximumMagnitude :: Double 
maximumMagnitude = 2
-- magnitude threshold estimates for seeing what I saw in these sets
-- 2 for mandelbrot
-- 9 for aquarium
-- 2 for lobster
-- 5 for badguy
-- 3 for sunset pyramid
-- 9 for alien
-- 9 for tsunami

maximumIterations :: Int 
maximumIterations = 30
-- 10 for alien

screenWidth :: Int
screenWidth = 300

screenHeight :: Int 
screenHeight = 300

fps :: Int
fps = 60