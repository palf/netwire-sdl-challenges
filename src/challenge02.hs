{-# LANGUAGE RankNTypes #-}

import Common.Drawing
import Common.Input
import Common.LifeCycle

import Control.Wire
import FRP.Netwire
import Prelude hiding ((.), id)


wireLoop :: (Monad m, Num a) => Session m s -> Wire s e m a a -> a -> (a -> m b) -> m c
wireLoop session wire x drawInstruction = loop' session wire x
    where
        loop' session' wire' x' = do
            (ds, session'') <- stepSession session'
            (ex, wire'') <- stepWire wire' ds (Right x')
            let x'' = either (const 0) id ex
            drawInstruction x''
            loop' session'' wire'' x''


-- drawWith :: forall a. Integral a => SDL.Renderer -> a -> IO ()
drawWith renderer x = runRender (drawSquareAt x' 100) renderer
    where x' = round x :: Int


drawSquareAt :: (Integral a) => a -> a -> Render ()
drawSquareAt x y = do
    setColor Red
    drawRect (x - 25) (y - 25) 50 50


input :: Wire (Timed NominalDiffTime ()) () IO Double Double
input = integral 0 . velocity


velocity :: forall a s. Wire s () IO a Double
velocity = pure (-20) . isKeyDown LeftKey
    <|> pure 20 . isKeyDown RightKey
    <|> pure 0


main :: IO ()
main = withSDLWindow ("Challenge 02", 200, 200) $ \renderer ->
    wireLoop clockSession_ input 0 (drawWith renderer)
