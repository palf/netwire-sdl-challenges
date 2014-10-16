{-# LANGUAGE RankNTypes #-}

import Common.LifeCycle
import Common.Drawing
import Common.Input

import Data.Either
import Control.Wire
import Prelude hiding ((.), id)


main :: IO ()
main = withSDLWindow ("Keys Example", 200, 200) $ \renderer ->
    wireLoop clockSession_ keyExample 10 (drawFunc renderer)


wireLoop :: (Monad m, Num a) => Session m s -> Wire s e m a a -> a -> (a -> m b) -> m c
wireLoop session wire x micro = do
    (ds, session') <- stepSession session
    (ex, wire') <- stepWire wire ds (Right x)
    let x' = either (const 50) id ex
    micro x'
    wireLoop session' wire' x' micro

--------------------------------

keyExample :: forall a s. Wire s () IO a Double
keyExample = isKeyDown LeftKey . pure 40 <|> pure 60

