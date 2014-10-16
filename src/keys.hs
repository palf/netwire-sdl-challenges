{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

import Common.LifeCycle
import Common.Drawing
import Common.Input

import Data.Either
import Control.Wire
import Prelude hiding ((.), id)

import qualified Graphics.UI.SDL as SDL


-- wireLoop :: Session IO (s -> Timed NominalDiffTime s) -> Wire (s -> Timed NominalDiffTime s) e IO Int Int -> (Int -> IO ()) -> IO b
wireLoop :: forall (m :: * -> *) s e a b c.
                  (Monad m, Num a, Num b) =>
                  Session m s -> Wire s e m a b -> (b -> m c) -> m c
wireLoop session wire macro = do
    (ds, session') <- stepSession session
    (ex, wire') <- stepWire wire ds (Right 4)
    let x' = either (const 10) id ex
    macro x'
    wireLoop session' wire' macro

--------------------------------

-- keyExample :: (HasTime t s) => Wire s () IO a Int
keyExample :: forall a s. Wire s () IO a Int
keyExample = isKeyDown LeftKey . pure 40 <|> pure 60


main :: IO ()
main = withSDLWindow ("Keys Example", 200, 200) $ \renderer -> do
    setColorRed renderer
    drawSquare renderer 4

    wireClock keyExample $ \x -> do
        return ()




wireClock :: forall b e s.
                   Wire (s -> Timed NominalDiffTime s) e IO Int Int
                   -> (Int -> IO b) -> IO b
wireClock = wireLoop clockSession


drawSquare :: SDL.Renderer -> Int -> IO ()
drawSquare renderer x = withBlankScreen renderer $ \r -> drawRect r x x 20 (20 :: Int)



-- Control.Category (.) :: Category cat => cat b c -> cat a b -> cat a c

