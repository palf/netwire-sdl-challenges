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
type Position = (Double, Double)


wireLoop :: (Monad m) => Session m s -> Wire s e m Position Position -> Position -> (Position -> m b) -> m c
wireLoop session wire x micro = do
    (ds, session') <- stepSession session
    (ex, wire') <- stepWire wire ds (Right x)
    let x' = either (const (0, 0)) id ex
    micro x'
    wireLoop session' wire' x' micro


drawWith :: SDL.Renderer -> Position -> IO ()
drawWith renderer (x, y) = runRender (drawSquareAt x' y') renderer
    where x' = round x :: Int
          y' = round y :: Int


drawSquareAt :: (Integral a) => a -> a -> Render ()
drawSquareAt x y = do
    setColor Red
    drawRect (x - 25) (y - 25) 50 50


leftAcc :: Wire s () IO a Double
leftAcc = pure 200 . isKeyDown LeftKey <|> pure 0


rightAcc :: Wire s () IO a Double
rightAcc = pure 200 . isKeyDown RightKey <|> pure 0


upAcc :: Wire s () IO a Double
upAcc = pure 200 . isKeyDown UpKey <|> pure 0


downAcc :: Wire s () IO a Double
downAcc = pure 200 . isKeyDown DownKey <|> pure 0


xAcceleration :: Wire s () IO a Double
xAcceleration = liftA (uncurry (-)) (rightAcc &&& leftAcc)


yAcceleration :: Wire s () IO a Double
yAcceleration = liftA (uncurry (-)) (downAcc &&& upAcc)


velocity :: Wire DiffTime () IO (Double, Bool) Double
velocity = integralWith bounce 0
    where bounce collisions v | collisions = (-v) * 0.95
                              | otherwise  =   v  * 0.99


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


positionFromAcceleration :: Wire DiffTime () IO a Double -> Wire DiffTime () IO a Double
positionFromAcceleration wire = proc x -> do
    accel <- wire -< x
    rec (position, collides) <- position -< velocity
        velocity <- velocity -< (accel, collides)
    returnA -< position


input :: Wire DiffTime () IO a Position
input = xInput &&& yInput
    where xInput = positionFromAcceleration xAcceleration
          yInput = positionFromAcceleration yAcceleration


main :: IO ()
main = withSDLWindow ("Challenge 05", 200, 200) $ \renderer ->
    wireLoop clockSession_ input (0,0) (drawWith renderer)
