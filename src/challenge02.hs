{-# LANGUAGE StandaloneDeriving #-}

import qualified FRP.Netwire as Netwire hiding (empty)
import qualified Graphics.UI.SDL as SDL

import Common.Drawing
import Common.LifeCycle
import Common.Input
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


challenge2 :: (Monad m, HasTime t s) => Wire s () m (Fishy) Double
challenge2 = Netwire.integral 0 . challenge2_velocity


challenge2_velocity :: (Monad m, Monoid e) => Wire s e m (Fishy) Double
challenge2_velocity  =  pure (-20) . when (keyDown LeftKey)
                    <|> pure 20 . when (keyDown RightKey)
                    <|> pure 0


main :: IO ()
main = withSDLWindow ("Challenge 02", 200, 200) $ \renderer -> do
    wireLoop clockSession challenge2 0 (drawFunc renderer)


wireLoop :: (Monad m, Num a) => Session m s -> Wire s e m a a -> a -> (a -> m b) -> m c
wireLoop session wire x micro = do
    (ds, session') <- stepSession session
    (ex, wire') <- stepWire wire ds (Right x)
    let x' = either (const 0) id ex
    micro x'
    wireLoop session' wire' x' micro


drawFunc :: (RealFrac a) => SDL.Renderer -> a -> IO ()
drawFunc renderer x = void $ withBlankScreen renderer (drawSquareAt x' 0)
    where x' = round x :: Int


-- drawFunc :: SDL.Renderer -> Double -> IO ()
-- drawFunc renderer x = do
--     keysDown' <- parseEvents keysDown
--     (ds, s') <- stepSession s
--     (ex, w') <- stepWire w ds (Right keysDown')
--     let x' = either (const 0) id ex

