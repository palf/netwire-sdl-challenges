module Common.LifeCycle (
    withSDLWindow
) where

import qualified Graphics.UI.SDL as SDL

import Control.Exception
import Control.Monad
import Data.Bits
import Foreign.C.String
import GHC.Word


withSDLWindow :: (String, Int, Int) -> (SDL.Renderer -> IO a) -> IO ()
withSDLWindow windowData renderOperation = do
    initializeSDL [SDL.SDL_INIT_EVERYTHING]

    window <- createWindow windowData
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED, SDL.SDL_RENDERER_PRESENTVSYNC]

    void $ finally (renderOperation renderer) (sdlCleanup window renderer)


initializeSDL :: [Word32] -> IO ()
initializeSDL flags = void $ SDL.init (bitwiseOr flags)


createWindow :: (String, Int, Int) -> IO SDL.Window
createWindow (windowTitle, windowWidth, windowHeight) = withCAString windowTitle $ \title ->
    SDL.createWindow title x y w h SDL.SDL_WINDOW_SHOWN
    where x = 0
          y = 0
          w = fromIntegral windowWidth
          h = fromIntegral windowHeight


createRenderer :: SDL.Window -> Int -> [Word32] -> IO SDL.Renderer
createRenderer window index flags = SDL.createRenderer window (fromIntegral index) (bitwiseOr flags)


bitwiseOr :: [Word32] -> Word32
bitwiseOr = foldl (.|.) 0


sdlCleanup :: SDL.Window -> SDL.Renderer -> IO ()
sdlCleanup window renderer = do
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
