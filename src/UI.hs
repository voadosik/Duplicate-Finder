module UI (launchUI) where

import Brick
import Brick.Main (App(..), halt, defaultMain, neverShowCursor)
import Brick.Types (Widget, BrickEvent(..), EventM)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Brick.AttrMap (attrName)
import qualified Graphics.Vty as V
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as Vec
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory (removeFile)
import Control.Exception (try, SomeException)
import Brick.Widgets.List (list, listMoveUp, listMoveDown, listSelectedElement)

type Name = String

data St = St 
  { stGroups :: [FileGroup], stCurrentGroup :: Int, stFileList :: GenericList Name Vec.Vector FilePath,
  stMarkedForDeletion :: Set FilePath, stMessage :: Maybe String}

type FileGroup = [FilePath]

initState :: [FileGroup] -> St
initState groups = St
  { stGroups = groups, stCurrentGroup = 0, stFileList = if null groups then list "files" Vec.empty 1 else list "files" (Vec.fromList $ head groups) 1,
   stMarkedForDeletion = Set.empty, stMessage = Nothing}

drawUI :: St -> [Widget Name]
drawUI st = 
  [ vBox 
    [ hBox 
      [ str "Group ", str $ show (stCurrentGroup st + 1), str " of ", str $ show (length $ stGroups st), str " | ",
       str $ show (Set.size $ stMarkedForDeletion st), str " files marked for deletion"], str " ",
       case stMessage st of
          Just msg -> withAttr (attrName "message") (str msg) <+> str " "
          Nothing -> str " ",
          borderWithLabel (str "Duplicate Files (Space = Mark, Enter = Delete Marked, q = Quit)") $
          renderList drawFileItem True (stFileList st), str " ", 
          str "Controls: ↑/↓ = Navigate files, ←/→ = Change between groups, Space = Mark/Unmark, Enter = Delete marked files, q = Quit"
    ]
  ]

drawFileItem :: Bool -> FilePath -> Widget Name
drawFileItem isSelected path =
  let marker = if isSelected then "→ " else "  " in str $ marker ++ path

app :: App St e Name
app = App
  { appDraw = drawUI, appHandleEvent = handleEvent, appStartEvent = return (),
    appAttrMap = const (attrMap V.defAttr 
      [ (attrName "selected", V.white `on` V.blue), (attrName "marked", V.black `on` V.yellow),
        (attrName "message", V.green `on` V.black), (listSelectedAttr, V.white `on` V.blue)]),
        appChooseCursor = neverShowCursor
  }

-- Arrows handlers

handleEvent :: BrickEvent Name e -> EventM Name St ()
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
  st <- get
  modify $ \s -> s { stFileList = listMoveUp (stFileList s)
                   , stMessage = Nothing }

handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
  st <- get
  modify $ \s -> s { stFileList = listMoveDown (stFileList s)
                   , stMessage = Nothing }

handleEvent (VtyEvent (V.EvKey V.KLeft [])) = do
  st <- get
  let newGroupIdx = max 0 (stCurrentGroup st - 1)
  if newGroupIdx /= stCurrentGroup st
    then do
      let newGroup = stGroups st !! newGroupIdx
      let newList = list "files" (Vec.fromList newGroup) 1
      put $ st { stCurrentGroup = newGroupIdx, stFileList = newList, stMessage = Nothing }
    else put $ st { stMessage = Nothing }

handleEvent (VtyEvent (V.EvKey V.KRight [])) = do
  st <- get
  let maxIdx = length (stGroups st) - 1
  let newGroupIdx = min maxIdx (stCurrentGroup st + 1)
  if newGroupIdx /= stCurrentGroup st
    then do
      let newGroup = stGroups st !! newGroupIdx
      let newList = list "files" (Vec.fromList newGroup) 1
      put $ st { stCurrentGroup = newGroupIdx, stFileList = newList, stMessage = Nothing }
    else put $ st { stMessage = Nothing }


-- space
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  st <- get
  case listSelectedElement (stFileList st) of
    Nothing -> put $ st { stMessage = Just "No file selected" }
    Just (_, filePath) -> do
      let marked = stMarkedForDeletion st
      let newMarked = if Set.member filePath marked
                      then Set.delete filePath marked
                      else Set.insert filePath marked
      let status = if Set.member filePath marked then "unmarked" else "marked"
      put $ st { stMarkedForDeletion = newMarked, stMessage = Just $ "File " ++ status ++ " for deletion" }

-- space(delete files)
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  if Set.null (stMarkedForDeletion st)
    then put $ st { stMessage = Just "No files marked for deletion" }
    else do
      results <- liftIO $ mapM tryDeleteFile (Set.toList $ stMarkedForDeletion st)
      let (success, fails) = partitionRes results
      let currentGroup = stGroups st !! stCurrentGroup st
      let remainingFiles = filter (\f -> not $ Set.member f (stMarkedForDeletion st)) currentGroup
      let updatedGroups = updateGroupAt (stCurrentGroup st) remainingFiles (stGroups st)
      let nonEmptyGroups = filter (not . null) updatedGroups
      
      if null nonEmptyGroups
        then do
          put $ st { stMessage = Just "All duplicates in the given directory are cleaned, press q to quit." }
        else do
          let newGroupId = min (stCurrentGroup st) (length nonEmptyGroups - 1)
          let newCurrentGroup = nonEmptyGroups !! newGroupId
          let newList = list "files" (Vec.fromList newCurrentGroup) 1
          
          let message = case (length success, length fails) of
                (s, 0) -> Just $ show s ++ " files deleted successfully"
                (0, f) -> Just $ "Failed to delete " ++ show f ++ " files"
                (s, f) -> Just $ show s ++ " files deleted, " ++ show f ++ " failed"
          
          put $ st { stGroups = nonEmptyGroups, stCurrentGroup = newGroupId, stFileList = newList,
                     stMarkedForDeletion = Set.empty, stMessage = message}

-- q key, quit
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ = return ()

tryDeleteFile :: FilePath -> IO (Either SomeException ())
tryDeleteFile path = try (removeFile path)

partitionRes :: [Either a b] -> ([b], [a])
partitionRes = foldr f ([], [])
  where
    f (Left e) (rs, ls) = (rs, e:ls)
    f (Right r) (rs, ls) = (r:rs, ls)

updateGroupAt :: Int -> a -> [a] -> [a]
updateGroupAt id newVal xs = take id xs ++ [newVal] ++ drop (id + 1) xs

launchUI :: [[FilePath]] -> IO ()
launchUI groups = 
  if null groups
    then putStrLn "No duplicates found"
    else void $ defaultMain app (initState groups)
