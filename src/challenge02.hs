{-# LANGUAGE StandaloneDeriving #-}

import Control.Monad (void)
import Control.Wire hiding (empty)
import Data.Bits
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import GHC.Word
import Graphics.UI.SDL.Types
import Prelude hiding ((.), id, null, filter)
import qualified FRP.Netwire as Netwire hiding (empty)
import qualified Graphics.UI.SDL as SDL


challenge2 :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) Double
challenge2 = Netwire.integral 0 . challenge2_velocity


challenge2_velocity :: (Monad m, Monoid e) => Wire s e m (Set SDL.Keysym) Double
challenge2_velocity  =  pure (-20) . when (keyDown SDL.SDLK_LEFT)
                    <|> pure 20 . when (keyDown SDL.SDLK_RIGHT)
                    <|> pure 0


main :: IO ()
main = withSDLWindow ("Challenge 02", 200, 200) $ do \renderer ->
    wireLoop clockSession challenge2 0 (drawFunc renderer)


wireLoop :: (HasTime t s) => Session IO s -> Wire s e IO Double Double -> Double -> (Double -> IO a) -> IO b
wireLoop session wire x micro = do
    (ds, session') <- stepSession session
    (ex, wire') <- stepWire wire ds (Right x)
    let x' = either (const 0) id ex
    micro x'
    wireLoop session' wire' x' micro


drawFunc :: SDL.Renderer -> Double -> IO ()
drawFunc renderer x = do
    keysDown' <- parseEvents keysDown
    (ds, s') <- stepSession s
    (ex, w') <- stepWire w ds (Right keysDown')
    let x' = either (const 0) id ex




parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
  event <- SDL.pollEvent
  case event of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k -> parseEvents (insert k keysDown)
    SDL.KeyUp k -> parseEvents (delete k keysDown)
    _ -> parseEvents keysDown

keyDown :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown k = not . null . filter ((== k) . SDL.symKey)

deriving instance Ord SDL.Keysym





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
