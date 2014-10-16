module Common.Drawing (
    drawFunc
) where


import qualified Graphics.UI.SDL as SDL

import Control.Monad (void)
import Foreign.Marshal.Utils (with)
import Graphics.UI.SDL.Types


withBlankScreen :: SDL.Renderer -> IO a -> IO ()
withBlankScreen renderer operation = do
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0x00
    SDL.renderClear renderer
    operation
    SDL.renderPresent renderer


setColorRed :: SDL.Renderer -> IO ()
setColorRed renderer = void $ SDL.setRenderDrawColor renderer 0xFF 0x00 0x00 0xFF


drawRect :: (Integral a) => SDL.Renderer -> a -> a -> a -> a -> IO ()
drawRect renderer x y w h = void $ with squareRect (SDL.renderFillRect renderer)
    where squareRect = toRect x y w h


toRect :: (Integral a) => a -> a -> a -> a -> SDL.Rect
toRect x y w h = SDL.Rect {
    rectX = fromIntegral x,
    rectY = fromIntegral y,
    rectW = fromIntegral w,
    rectH = fromIntegral h }




drawFunc :: (RealFrac a) => SDL.Renderer -> a -> IO ()
drawFunc renderer x = void $ withBlankScreen renderer (drawSquareAt x' 0 renderer)
    where x' = round x :: Int


drawSquareAt :: (Integral a) => a -> a -> SDL.Renderer -> IO ()
drawSquareAt x y renderer = do
    setColorRed renderer
    drawRect renderer x y 50 50
