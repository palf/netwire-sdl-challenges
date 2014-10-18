{-# LANGUAGE RankNTypes #-}

import Common.Drawing
import Common.Input
import Common.LifeCycle

import Control.Wire
import Prelude hiding ((.), id)


wireLoop :: (Monad m, Num a) => Session m s -> Wire s e m a a -> a -> (a -> m b) -> m c
wireLoop session wire x micro = do
    (ds, session') <- stepSession session
    (ex, wire') <- stepWire wire ds (Right x)
    let x' = either (const 50) id ex
    micro x'
    wireLoop session' wire' x' micro


-- drawWith :: forall a. Integral a => SDL.Renderer -> a -> IO ()
drawWith renderer x = runRender (drawSquareAt x 100) renderer


drawSquareAt :: (Integral a) => a -> a -> Render ()
drawSquareAt x y = do
    setColor Red
    drawRect (x - 25) (y - 25) 50 50


horizontal :: forall a s. Wire s () IO a Int
horizontal = pure 50 . isKeyDown LeftKey
    <|> pure 150 . isKeyDown RightKey
    <|> pure 100


vertical :: forall a s. Wire s () IO a Int
vertical = pure 50 . isKeyDown UpKey
    <|> pure 150 . isKeyDown DownKey
    <|> pure 100


input = horizontal


main :: IO ()
main = withSDLWindow ("Keys Example", 200, 200) $ \renderer ->
    wireLoop clockSession_ input rest (drawWith renderer)
    where rest = 100 :: Int
