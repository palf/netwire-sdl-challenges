module Common.Drawing (
    makeDrawing,
    withBlankScreen,
    setColorRed,
    drawRect,
    Render
) where

import qualified Graphics.UI.SDL as SDL

import Control.Monad.Reader
import Foreign.Marshal.Utils (with)
import Graphics.UI.SDL.Types


type Render a = ReaderT SDL.Renderer IO a


makeDrawing :: Render a -> SDL.Renderer -> IO ()
makeDrawing operation = runReaderT (withBlankScreen operation)


withBlankScreen :: Render a -> Render ()
withBlankScreen operation = do
    renderer <- ask
    liftIO $ SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0x00 >>
             SDL.renderClear renderer
    operation
    liftIO $ SDL.renderPresent renderer


setColorRed :: Render ()
setColorRed = do
    renderer <- ask
    void . liftIO $ SDL.setRenderDrawColor renderer 0xFF 0x00 0x00 0xFF


drawRect :: (Integral a) => a -> a -> a -> a -> Render ()
drawRect x y w h = do
    renderer <- ask
    void . liftIO $ with squareRect (SDL.renderFillRect renderer)
    where squareRect = toRect x y w h


toRect :: (Integral a) => a -> a -> a -> a -> SDL.Rect
toRect x y w h = SDL.Rect {
    rectX = fromIntegral x,
    rectY = fromIntegral y,
    rectW = fromIntegral w,
    rectH = fromIntegral h }
