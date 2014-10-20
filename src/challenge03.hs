{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}

import Common.Drawing
import Common.Input
import Common.LifeCycle

import Control.Wire hiding (empty)
import FRP.Netwire
import Prelude hiding ((.), id, null, filter)


type DiffTime = Timed NominalDiffTime ()


wireLoop :: (Monad m, Num a) => Session m s -> Wire s e m a a -> a -> (a -> m b) -> m c
wireLoop session wire x micro = do
    (ds, session') <- stepSession session
    (ex, wire') <- stepWire wire ds (Right x)
    let x' = either (const 0) id ex
    micro x'
    wireLoop session' wire' x' micro


-- drawWith :: forall a. (RealFrac a, Show a) => SDL.Renderer -> a -> IO ()
drawWith renderer x = runRender (drawSquareAt x' 100) renderer
    where x' = round x :: Int


drawSquareAt :: (Integral a) => a -> a -> Render ()
drawSquareAt x y = do
    setColor Red
    drawRect (x - 25) (y - 25) 50 50


acceleration :: Wire s () IO a Double
acceleration = pure (-200) . isKeyDown LeftKey
    <|> pure 200 . isKeyDown RightKey
    <|> pure 0


velocity :: Wire DiffTime () IO (Double, Bool) Double
velocity = integralWith bounce 0
    where bounce collisions v | collisions = -v
                              | otherwise  = v


position :: Wire DiffTime () IO Double (Double, Bool)
position = integralWith'' (clamp 50 150) 100


clamp :: (Ord a) => a -> a -> a -> (a, Bool)
clamp lower upper x
    | lower > upper     = clamp upper lower x
    | x < lower         = (lower, True)
    | x > upper         = (upper, True)
    | otherwise         = (x, False)


integralWith'' :: (Fractional a, HasTime t s) => (a -> (a, o)) -> a -> Wire s e m a (a, o)
integralWith'' correct = loop'
  where
    loop' x =
        mkPure $ \ds dx ->
            let dt = realToFrac (dtime ds)
                (x', b) = correct (x + dt * dx)
            in x `seq` (Right (x, b), loop' x')


input :: Wire DiffTime () IO a Double
input = proc x -> do
    accel <- acceleration -< x
    rec (position, collides) <- position -< velocity
        velocity <- velocity -< (accel, collides)
    returnA -< position


main :: IO ()
main = withSDLWindow ("Challenge 03", 200, 200) $ \renderer ->
    wireLoop clockSession_ input 0 (drawWith renderer)
