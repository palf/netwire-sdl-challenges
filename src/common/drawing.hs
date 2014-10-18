module Common.Drawing (
    runRender,
    withBlankScreen,
    setColor,
    drawRect,
    Render,
    Color (..)
) where

import qualified Graphics.UI.SDL as SDL

import Control.Monad.Reader
import Foreign.Marshal.Utils (with)
import qualified Graphics.UI.SDL.Types as Types


type Render = ReaderT SDL.Renderer IO
data Color = Red


runRender :: Render a -> SDL.Renderer -> IO ()
runRender operation = runReaderT (withBlankScreen operation)


withBlankScreen :: Render a -> Render ()
withBlankScreen operation = do
    renderer <- ask
    liftIO $ SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0x00 >>
             SDL.renderClear renderer
    operation
    liftIO $ SDL.renderPresent renderer


setColor :: Color -> Render ()
setColor Red = ask >>= \renderer -> void . liftIO $ SDL.setRenderDrawColor renderer 0xFF 0x00 0x00 0xFF


drawRect :: (Integral a) => a -> a -> a -> a -> Render ()
drawRect x y w h = do
    renderer <- ask
    void . liftIO $ with squareRect (SDL.renderFillRect renderer)
    where squareRect = toRect x y w h


toRect :: (Integral a) => a -> a -> a -> a -> SDL.Rect
toRect x y w h = SDL.Rect {
    Types.rectX = fromIntegral x,
    Types.rectY = fromIntegral y,
    Types.rectW = fromIntegral w,
    Types.rectH = fromIntegral h }
