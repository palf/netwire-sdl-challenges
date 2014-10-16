{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Common.Input (
    Key (..),
    isKeyDown,
    checkForKey
) where

import qualified Graphics.UI.SDL as SDL

import Control.Monad hiding (when)
import Control.Wire
import Data.Either
import Data.List
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek)
import Prelude hiding ((.), id)



data Key = LeftKey | RightKey | NoKey deriving (Show, Eq, Ord, Read)


instance Enum Key where
  fromEnum LeftKey = 79
  fromEnum RightKey = 80
  fromEnum NoKey = 0

  toEnum 79 = RightKey
  toEnum 80 = LeftKey
  toEnum _ = NoKey


getKeyState :: IO [Int]
getKeyState = alloca $ \numkeysPtr -> do
    keysPtr <- SDL.getKeyboardState numkeysPtr
    numkeys <- peek numkeysPtr
    wordset <- peekArray (fromIntegral numkeys) keysPtr

    let stuff = map fromIntegral wordset
    let things = elemIndices (1::Int) stuff

    -- print keysPtr
    return things


checkForKey :: Key -> IO Bool
checkForKey key = elem (fromEnum key) <$> getKeyState


keyCheck :: Key -> a -> IO (Either () Int)
keyCheck key x = do
    -- print x
    s <- checkForKey key
    return $ case s of
        True   -> Right 50
        False  -> Left ()


isKeyDown :: Key -> Wire s () IO a Int
isKeyDown key = mkGen_ $ keyCheck key
