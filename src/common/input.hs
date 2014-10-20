{-# LANGUAGE RankNTypes #-}

module Common.Input (
    Key (..),
    isKeyDown,
    isKeyUp
) where

import qualified Graphics.UI.SDL as SDL

import Control.Applicative
import Control.Monad hiding (when)
import Control.Monad.Trans
import Control.Wire
import Data.Either
import Data.List
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek)
import Prelude hiding ((.), id)


data Key
    = EscapeKey
    | RightKey
    | LeftKey
    | DownKey
    | UpKey
    | NoKey
    deriving (Show, Eq, Ord, Read)


instance Enum Key where
    fromEnum EscapeKey = 41
    fromEnum RightKey = 79
    fromEnum LeftKey = 80
    fromEnum DownKey = 81
    fromEnum UpKey = 82
    fromEnum NoKey = 0

    toEnum 41 = EscapeKey
    toEnum 79 = RightKey
    toEnum 80 = LeftKey
    toEnum 81 = DownKey
    toEnum 82 = UpKey
    toEnum _ = NoKey


getKeyState :: IO [Int]
getKeyState = do
    SDL.pumpEvents -- not necessary if we consume the event list
    alloca $ \numkeysPtr -> do
        keysPtr <- SDL.getKeyboardState numkeysPtr
        numkeys <- peek numkeysPtr
        wordset <- peekArray (fromIntegral numkeys) keysPtr

        return $ elemIndices (1::Int) (map fromIntegral wordset)


keyIsDown :: (MonadIO m, Functor m) => Key -> m Bool
keyIsDown key = elem (fromEnum key) <$> liftIO getKeyState


select :: a -> b -> Bool -> Either a b
select l r p = if p then Right r else Left l


isKeyDown :: (Functor m, MonadIO m, Monoid e) => Key -> Wire s e m a Key
isKeyDown key = mkGen_ $ \_ -> liftM (select mempty key) (keyIsDown key)


isKeyUp :: (Functor m, MonadIO m, Monoid e) => Key -> Wire s e m a Key
isKeyUp key = mkGen_ $ \_ -> liftM (select mempty key) (not <$> keyIsDown key)

