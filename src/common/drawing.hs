module Common.Drawing (
    withBlankScreen,
    setColorRed,
    drawRect
) where


import qualified Graphics.UI.SDL as SDL

import Control.Monad (void)
import Foreign.Marshal.Utils (with)
import Graphics.UI.SDL.Types


withBlankScreen :: SDL.Renderer -> (SDL.Renderer -> IO a) -> IO ()
withBlankScreen renderer operation = do
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    SDL.renderClear renderer
    operation renderer
    SDL.renderPresent renderer


setColorRed :: SDL.Renderer -> IO ()
setColorRed renderer = void $ SDL.setRenderDrawColor renderer 0xFF 0x00 0x00 0xFF


drawRect :: (Integral a) => SDL.Renderer -> a -> a -> a -> a -> IO ()
drawRect renderer x y w h = do
    void $ with squareRect (SDL.renderFillRect renderer)
    where squareRect = toRect x y w h


toRect :: (Integral a) => a -> a -> a -> a -> SDL.Rect
toRect x y w h = SDL.Rect {
    rectX = fromIntegral x,
    rectY = fromIntegral y,
    rectW = fromIntegral w,
    rectH = fromIntegral h }
