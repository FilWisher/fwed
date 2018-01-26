{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Editor.Buffer
    ( Handle
    
    , new
    , fromFile

    , insertChunk
    , deleteChunk
    , replaceChunk
    ) where

import qualified Data.Text as T
import qualified Yi.Rope as Rope
import Lens.Micro ((^.), (.~), (&), (%~), ix, _1)
import Lens.Micro.TH (makeLenses)

import qualified System.IO as System

data Handle = Handle
  { _text :: Rope.YiString
  , _pos :: (Int, Int)
  }
  deriving (Show)
$(makeLenses ''Handle)

new :: Handle
new = Handle Rope.empty (0, 0)

-- TODO: need to handle exceptions for non-existent file (return new file)
fromFile :: System.FilePath -> IO Handle
fromFile path =
  Rope.readFile path >>= \case
    Left err -> do
      print err
      return new
    Right (rope, _) -> 
      return $ Handle rope (0, 0)

insertChunk :: Handle -> T.Text -> Handle
insertChunk handle chunk =
  handle & text .~ (Rope.unlines lines')
  where
    (col, row) = handle ^. pos
    lines = Rope.lines (handle ^. text)
    lines' = lines & ix row %~ insertChunkLine chunk col

insertChunkLine :: T.Text -> Int -> Rope.YiString -> Rope.YiString
insertChunkLine chunk pos buffer = 
  before `Rope.append` Rope.fromText chunk `Rope.append` after
  where
    (before, after) = Rope.splitAt pos buffer

deleteChunk :: Handle -> (Int, Int) -> Handle
deleteChunk = editSection deleteChunkPos

coordToPos :: (Int, Int) -> Rope.YiString -> Int
coordToPos (col, row) = (col +) . go row 0 . Rope.lines
  where
    go 0 sum _ = sum
    go _ sum [] = sum
    go row sum (line:lines) = go (row-1) (sum + Rope.length line + 1) lines

deleteChunkPos :: Int -> Int -> Rope.YiString -> Rope.YiString
deleteChunkPos start end buffer =
  before `Rope.append` after
  where
    (before, _) = Rope.splitAt start buffer
    (_, after) = Rope.splitAt end buffer

editSection :: (Int -> Int -> Rope.YiString -> Rope.YiString) -> Handle -> (Int, Int) -> Handle
editSection editFn handle fromCoord =
  handle & text %~ editFn (min from to) (max from to)
  where
    buffer = handle ^. text
    from = coordToPos fromCoord buffer
    to = coordToPos (handle ^. pos) buffer

replaceChunk :: Handle -> T.Text -> (Int, Int) -> Handle
replaceChunk handle chunk fromCoord =
  editSection (replaceChunkPos chunk) handle fromCoord

replaceChunkPos :: T.Text -> Int -> Int -> Rope.YiString -> Rope.YiString
replaceChunkPos chunk start end buffer =
  before `Rope.append` Rope.fromText chunk `Rope.append` after
  where
    (before, _) = Rope.splitAt start buffer
    (_, after) = Rope.splitAt end buffer
