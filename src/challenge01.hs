import Prelude hiding ((.), id)

import Control.Monad
import Control.Wire
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import GHC.Word
import Graphics.UI.SDL.Types
import qualified FRP.Netwire as Netwire
import qualified Graphics.UI.SDL as SDL



-- challenge1 :: (, Monad m) => Wire s e m Double Double
challenge1 :: (Monad m, HasTime t s) => Wire s e m Double Double
challenge1 = Netwire.integral 0 + pure 2


main :: IO ()
main = withSDLWindow ("Challenge 01", 200, 200) $ \renderer ->
    wireLoop clockSession_ challenge1 0 (drawFunc renderer)


wireLoop :: (HasTime t s) => Session IO s -> Wire s e IO Double Double -> Double -> (Double -> IO a) -> IO b
wireLoop session wire x micro = do
    (ds, session') <- stepSession session
    (ex, wire') <- stepWire wire ds (Right x)
    let x' = either (const 0) id ex
    micro x'
    wireLoop session' wire' x' micro



drawFunc :: SDL.Renderer -> Double -> IO ()
drawFunc renderer x = void $ withBlankScreen renderer (drawSquareAt x 0)



squareRect :: (Integral a) => a -> SDL.Rect
squareRect x = SDL.Rect {
    rectX = fromIntegral x,
    rectY =  0,
    rectW = 50,
    rectH = 50 }


drawSquareAt :: Double -> Double -> SDL.Renderer -> IO ()
drawSquareAt x y renderer = do
    SDL.setRenderDrawColor renderer 0xFF 0x00 0x00 0xFF
    void $ with (squareRect (round x)) (SDL.renderFillRect renderer)


withBlankScreen :: SDL.Renderer -> (SDL.Renderer -> IO a) -> IO ()
withBlankScreen renderer operation = do
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    SDL.renderClear renderer
    operation renderer
    SDL.renderPresent renderer








withSDLWindow :: (String, Int, Int) -> (SDL.Renderer -> IO a) -> IO ()
withSDLWindow windowData renderOperation = do
    initializeSDL [SDL.initFlagVideo]

    window <- createWindow windowData
    renderer <- createRenderer window (-1) [SDL.rendererFlagAccelerated, SDL.rendererFlagPresentVSync]

    renderOperation renderer
    sdlCleanup window renderer


sdlCleanup :: SDL.Window -> SDL.Renderer -> IO ()
sdlCleanup window renderer = do
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit


initializeSDL :: [Word32] -> IO ()
initializeSDL flags = void $ SDL.init (bitwiseOr flags)


createWindow :: (String, Int, Int) -> IO SDL.Window
createWindow (windowTitle, windowWidth, windowHeight) = withCAString windowTitle $ \title ->
    SDL.createWindow title x y w h SDL.windowFlagShown
    where x = SDL.windowPosUndefined
          y = SDL.windowPosUndefined
          w = fromIntegral windowWidth
          h = fromIntegral windowHeight


-- createRenderer :: SDL.Window -> CInt -> [Word32] -> IO SDL.Renderer
createRenderer window index flags = SDL.createRenderer window index (bitwiseOr flags)


bitwiseOr :: [Word32] -> Word32
bitwiseOr = foldl (.|.) 0
