{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Editor.Screen where

import Data.Monoid ((<>))
import Data.List (find)
import qualified Data.Text as T
import qualified Editor.Buffer as Buffer
import qualified Brick
import qualified Brick.Main as M
import qualified Brick.AttrMap as A
import qualified Brick.Types as Types
import qualified Graphics.Vty as V

import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.), (^?), ix, _1, _2)

data Buffer
  = Buffer
  { _text :: T.Text
  , _pos :: (Int, Int)
  }
  deriving (Show)

data State
  = State
  { _buffers :: [Buffer]
  , _current :: Int
  }
  deriving (Show)

$(makeLenses ''Buffer)
$(makeLenses ''State)

newtype Handle
  = Handle
  { buf :: Buffer.Handle
  }

new :: Buffer.Handle -> Handle
new = Handle

drawEditor :: State -> Types.Widget Name
drawEditor st = Brick.showCursor () (Types.Location $ (currentBuffer st) ^. pos) $ Brick.txt ((currentBuffer st) ^. text)

drawUI :: State -> [Types.Widget Name]
drawUI st = [drawEditor st]

type Name = ()

currentBuffer :: State -> Buffer
currentBuffer st = (st ^. buffers) !! (st ^. current)

appEvent :: State -> Types.BrickEvent Name e -> Types.EventM Name (Types.Next State)
appEvent st (Types.VtyEvent ev) =
    case ev of
        V.EvKey (V.KFun 4) [] -> M.halt st
        V.EvKey V.KEnter [] -> M.continue $ (st & buffers . ix (st ^. current) %~ insertNewline)
        V.EvKey V.KBS [] -> M.continue $ (st & buffers . ix (st ^. current) %~ (flip deleteChar 1))
        V.EvKey (V.KChar '\t') [] -> M.continue $ nextBuffer st
        V.EvKey (V.KChar '`') [] -> M.halt st
        V.EvKey V.KBackTab [] -> M.continue $ prevBuffer st
        V.EvKey (V.KChar 'n') x -> M.continue $ newBuffer st
        V.EvKey (V.KChar c) [] -> M.continue $ (st & buffers . ix (st ^. current) %~ (flip insertChar c))
        V.EvKey V.KLeft [] -> M.continue $ (st & buffers . ix (st ^. current) . pos %~ left 1)
        V.EvKey V.KRight [] -> M.continue $ (st & buffers . ix (st ^. current) %~ right 1)
        _ -> M.continue $ st
    where
      join a b c = a <> b <> c
appEvent st _ = M.continue st

nextBuffer :: State -> State
nextBuffer st = st { _current = ((st ^. current) + 1) `mod` (length $ st ^. buffers) }

prevBuffer :: State -> State
prevBuffer st = st { _current = if prev < 0 then len - 1 else prev }
  where
    prev = ((st ^. current) - 1)
    len = length $ st ^. buffers
    
newBuffer :: State -> State
newBuffer st = st { _buffers = (st ^. buffers) ++ [Buffer "" (0, 0)], _current = length (st ^. buffers) - 1 }

insertNewline :: Buffer -> Buffer
insertNewline buf = down 1 $ (buf & text .~ text' & pos . _1 .~ 0)
  where
    (p, _) = buf ^. pos
    (before, after) = T.splitAt p (buf ^. text)
    text' = before `T.append` (T.cons '\n' after)

insertChar :: Buffer -> Char -> Buffer
insertChar buf c = right 1 $ buf & text .~ text' 
  where
    (p, _) = buf ^. pos
    (before, after) = T.splitAt p (buf ^. text)
    text' = before `T.append` (T.cons c after)

deleteChar :: Buffer -> Int -> Buffer
deleteChar buf n = buf
  { _text = text'
  , _pos = left n (buf ^. pos)
  }
  where
    (p, _) = buf ^. pos
    (before, _) = T.splitAt (p-n) (buf ^. text)
    (_, after) = T.splitAt p (buf ^. text)
    text' = before `T.append` after

right :: Int -> Buffer -> Buffer
right n buf = buf & pos . _1 %~ (+dx)
  where
    dx = 
      if (buf ^. (pos . _1)) + n < T.length (buf ^. text) + 1
        then n 
        else 0

down :: Int -> Buffer -> Buffer
down n buf = buf & pos . _2 %~ (+dy)
  where
    dy = 
      if (buf ^. (pos . _2)) + n < length (filter (=='\n') $ T.unpack $ buf ^. text) + 1
        then n 
        else 0

left :: Int -> (Int, Int) -> (Int, Int)
left n (x, y) = (max (x - n) 0, y)

initialState :: State
initialState = State [buf] 0
  where
    buf = Buffer
      { _text = ""
      , _pos  = (0, 0)
      }

app :: M.App State e Name
app = M.App 
  { M.appDraw         = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent  = appEvent
  , M.appStartEvent   = return
  , M.appAttrMap      = const $ A.attrMap V.defAttr []
  }

main :: IO ()
main = do
    st <- M.defaultMain app initialState
    putStrLn "In input 1 you entered:\n"
    print (st ^. buffers ^? ix (st ^. current))
