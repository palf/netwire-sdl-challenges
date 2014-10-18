{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}

import Common.Drawing
import Common.Input
import Common.LifeCycle

import Control.Wire hiding (empty)
import FRP.Netwire
import Prelude hiding ((.), id, null, filter)


import qualified Graphics.UI.SDL as SDL


type DiffTime = Timed NominalDiffTime ()


wireLoop :: (Monad m, Num a) => Session m s -> Wire s e m a a -> a -> (a -> m b) -> m c
wireLoop session wire x micro = do
    (ds, session') <- stepSession session
    (ex, wire') <- stepWire wire ds (Right x)
    let x' = either (const 0) id ex
    micro x'
    wireLoop session' wire' x' micro


drawWith :: forall a. (RealFrac a, Show a) => SDL.Renderer -> a -> IO ()
drawWith renderer x = do
    print x
    runRender (drawSquareAt x' 100) renderer
    where x' = round x :: Int


drawSquareAt :: (Integral a) => a -> a -> Render ()
drawSquareAt x y = do
    setColor Red
    drawRect (x - 25) (y - 25) 50 50



collided :: (Ord a, Num a) => (a, a) -> a -> (a, Bool)
collided (a, b) x
  | x < a = (a, True)
  | x > b = (b, True)
  | otherwise = (x, False)


-- position :: (Monad m, HasTime t s) => Wire s e m Double (Double, Bool)
-- position = integral 0 >>> (arr $ collided (0, 150))

-- arr $ collided (0, 150) :: Arrow ? a (a, Bool)


position = integralWith' clamp 0
    where
        clamp p | p < 0 || p > 150 = (max 1 (min 149 p), True)
                | otherwise        = (p, False)


dampen :: (Fractional a, HasTime t s) => a -> Wire s e m a a
dampen rate = integralWith' (rate *) 0


constrain :: Double -> Double -> Double -> Double
constrain lower upper x
    | lower > upper = constrain upper lower x
    | x < lower     = lower
    | x > upper     = upper
    | otherwise     = x


limit :: Double -> Double -> Wire DiffTime () IO Double Double
limit lower upper = mkSF_ $ constrain lower upper


type Stuff = Int

acceleration :: forall a s. Wire s () IO Stuff Double
acceleration = pure (-200) . isKeyDown LeftKey
    <|> pure 200 . isKeyDown RightKey
    <|> pure 0


velocity :: forall a. Wire DiffTime () IO (Double, Bool) Double
velocity = integralWith bounce (0, False)
-- velocity   (a) Double >>> Double (Double)  =  a Double


bounce :: (w -> a -> a) -> a
bounce w a = a


integralWith' :: (Fractional a, HasTime t s) => (a -> a) -> a -> Wire s e m a a
integralWith' correct = loop'
    where
    loop' x' =
        mkPure $ \ds dx ->
            let dt = realToFrac (dtime ds)
                x = correct (x' + dt * dx)
            in x' `seq` (Right x', loop' x)


position :: forall a. Wire DiffTime () IO Double (Double, Bool)
position = mkSF_ $ \x ->
    if x > 200 then (200, True) else (x, False)


input :: forall a. Wire DiffTime () IO a Double
input = proc x -> do
    accel <- acceleration -< x
    rec (position, collides) <- position -< velocity
        velocity <- velocity -< (accel, collides)
    returnA -< position




main :: IO ()
main = withSDLWindow ("Challenge 03", 200, 200) $ \renderer ->
    wireLoop clockSession_ input 0 (drawWith renderer)
