import qualified FRP.Netwire as Netwire

import Common.Drawing
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
drawWith renderer x = makeDrawing (drawSquareAt x' 100) renderer
    where x' = round x :: Int


drawSquareAt :: (Integral a) => a -> a -> Render ()
drawSquareAt x y = do
    setColorRed
    drawRect (x - 25) (y - 25) 50 50


input :: (Monad m, HasTime t s) => Wire s e m Double Double
input = Netwire.integral 0 + pure 8


main :: IO ()
main = withSDLWindow ("Challenge 01", 200, 200) $ \renderer ->
    wireLoop clockSession_ input 10 (drawWith renderer)
