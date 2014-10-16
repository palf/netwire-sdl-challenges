import qualified FRP.Netwire as Netwire

import Common.Drawing
import Common.LifeCycle
import Control.Monad
import Control.Wire
import Prelude hiding ((.), id)


main :: IO ()
main = withSDLWindow ("Challenge 01", 200, 200) $ \renderer ->
    wireLoop clockSession_ challenge01 10 (drawFunc renderer)


challenge01 :: (Monad m, HasTime t s) => Wire s e m Double Double
challenge01 = Netwire.integral 0 + pure 8


wireLoop :: (Monad m, Num a) => Session m s -> Wire s e m a a -> a -> (a -> m b) -> m c
wireLoop session wire x micro = do
    (ds, session') <- stepSession session
    (ex, wire') <- stepWire wire ds (Right x)
    let x' = either (const 50) id ex
    micro x'
    wireLoop session' wire' x' micro
