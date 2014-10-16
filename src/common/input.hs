{-# LANGUAGE RankNTypes #-}

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



data Key
    = RightKey
    | LeftKey
    | DownKey
    | UpKey
    | NoKey
    deriving (Show, Eq, Ord, Read)


instance Enum Key where
  fromEnum RightKey = 79
  fromEnum LeftKey = 80
  fromEnum DownKey = 81
  fromEnum UpKey = 82
  fromEnum NoKey = 0

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


checkForKey :: Key -> IO Bool
checkForKey key = elem (fromEnum key) <$> getKeyState


keyCheck :: (Show a) => Key -> a -> IO (Either () Double)
keyCheck key x = do
    print x
    liftM (select () 0) (checkForKey key)


select :: a -> b -> Bool -> Either a b
select l r p = if p then Right r else Left l


isKeyDown :: (Show a) => Key -> Wire s () IO a Double
isKeyDown key = mkGen_ $ keyCheck key
