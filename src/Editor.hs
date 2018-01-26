module Editor
  ( Handle(..)
  ) where

import qualified Editor.Screen as Screen
import qualified Editor.Buffer as Buffer
import qualified Data.Vector as V

data Handle
  = Handle
  { screens :: V.Vector Screen.Handle
  , current :: Int
  }

nextScreen :: Handle -> Handle
nextScreen editor =
  case screens editor V.!? curr of
    Just _ -> editor { current = curr + 1 }
    Nothing -> editor
  where
    curr = current editor
