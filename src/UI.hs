module UI (launchUI) where

import Brick
import Brick.Main (App(..), halt, defaultMain, neverShowCursor)
import Brick.Types (Widget, BrickEvent(..), EventM)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.AttrMap (attrName)
import qualified Graphics.Vty as V
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as Vec
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory (removeFile)
import Control.Exception (try, SomeException)
import Brick.Widgets.List 

type Name = String

data St = St 
  { stGroups :: [FileGroup],  -- All duplicate groups
    stCurrentGroup :: Int,    -- Index of selected group
    stFileList :: GenericList Name Vec.Vector FilePath,   -- Brick's scrollable list of files
    stMarkedForDeletion :: Set FilePath, -- Set of files marked for deletion
    stMessage :: Maybe String -- Status or error message
  }

type FileGroup = [FilePath]

-- Set the initial state of the duplicate groups
initState :: [FileGroup] -> St
initState groups = St
  { stGroups = groups, stCurrentGroup = 0, 
    stFileList = if null groups then list "files" Vec.empty 1 
      else list "files" (Vec.fromList $ head groups) 1,
    stMarkedForDeletion = Set.empty, stMessage = Nothing}

-- Draws UI with current group of files, list of files, message and usage guide
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
          -- render the scrollable file list, true parameter allows item highlighting, star if marked
          renderList (drawFileItem (stMarkedForDeletion st)) True (stFileList st), 
          str "Controls: ↑/↓ = Navigate files, ←/→ = Change between groups, Space = Mark/Unmark, Enter = Delete marked files, q = Quit"
    ]
  ]

-- Draws a single file in the file list
drawFileItem :: Set FilePath -> Bool -> FilePath -> Widget Name
drawFileItem markedSet isSelected path =
  let marker = if isSelected then "→ " else "  " 
      star = if Set.member path markedSet then "* " else "  "
  in str $ marker ++ star ++ path

-- Brick application definition(assignment of methods to internal definitions of Brick library)
app :: App St e Name
app = App
  { appDraw = drawUI, appHandleEvent = handleEvent, appStartEvent = return (),
    appAttrMap = const (attrMap V.defAttr 
      [ (attrName "selected", V.white `on` V.blue), (attrName "marked", V.black `on` V.yellow),
        (attrName "message", V.green `on` V.black), (listSelectedAttr, V.white `on` V.blue)]),
        appChooseCursor = neverShowCursor
  }

-- UI handlers

-- Move selection up
handleEvent :: BrickEvent Name e -> EventM Name St ()
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
  _ <- get
  modify $ \s -> s { stFileList = listMoveUp (stFileList s)
                   , stMessage = Nothing }


-- Move selection down
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
  _ <- get
  modify $ \s -> s { stFileList = listMoveDown (stFileList s)
                   , stMessage = Nothing }


-- Move to previous group of duplcate files
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = do
  st <- get
  let newGroupIdx = max 0 (stCurrentGroup st - 1)
  if newGroupIdx /= stCurrentGroup st
    then do
      -- Fetch the new group of files and create a new scrollable list
      let newGroup = stGroups st !! newGroupIdx
      let newList = list "files" (Vec.fromList newGroup) 1
      put $ st { stCurrentGroup = newGroupIdx, stFileList = newList, stMessage = Nothing }
    else put $ st { stMessage = Nothing }


-- Move to next group of duplcate files
handleEvent (VtyEvent (V.EvKey V.KRight [])) = do
  st <- get
  let maxIdx = length (stGroups st) - 1
  let newGroupIdx = min maxIdx (stCurrentGroup st + 1)
  if newGroupIdx /= stCurrentGroup st
    then do
      -- Fetch the new group of files and create a new scrollable list
      let newGroup = stGroups st !! newGroupIdx
      let newList = list "files" (Vec.fromList newGroup) 1
      put $ st { stCurrentGroup = newGroupIdx, stFileList = newList, stMessage = Nothing }
    else put $ st { stMessage = Nothing }


-- Space key
-- Mark or unmark file for deletion
-- Updates message
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

-- Enter key
-- Delete all the files marked for deletion
-- Updates the group lists after deletions
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  if Set.null (stMarkedForDeletion st)
    then put $ st { stMessage = Just "No files marked for deletion" }
    else do
      -- Try to delete every marked file
      results <- liftIO $ mapM tryDeleteFile (Set.toList $ stMarkedForDeletion st)
      -- Split into successes and fails
      let (success, fails) = partitionRes results
      let currentGroup = stGroups st !! stCurrentGroup st
      -- Removes deleted files from the current group
      let remainingFiles = filter (\f -> not $ Set.member f (stMarkedForDeletion st)) currentGroup
      
      let updatedGroups = updateGroupAt (stCurrentGroup st) remainingFiles (stGroups st)
      
      -- Keep only groups with at least 1 element
      let nonEmptyGroups = filter (not . null) updatedGroups
      
      if null nonEmptyGroups
        then do
          put $ st { stMessage = Just "All duplicates in the given directory are cleaned, press q to quit." }
        else do
          -- ensure we dont go out of bounds
          let newGroupId = min (stCurrentGroup st) (length nonEmptyGroups - 1)
          let newCurrentGroup = nonEmptyGroups !! newGroupId
          let newList = list "files" (Vec.fromList newCurrentGroup) 1
          
          -- Build a return message( number of successes and fails)
          let message = case (length success, length fails) of
                (s, 0) -> Just $ show s ++ " files deleted successfully"
                (0, f) -> Just $ "Failed to delete " ++ show f ++ " files"
                (s, f) -> Just $ show s ++ " files deleted, " ++ show f ++ " failed"
          
          put $ st { stGroups = nonEmptyGroups, stCurrentGroup = newGroupId, stFileList = newList,
                     stMarkedForDeletion = Set.empty, stMessage = message}

-- q key
-- Halt the application
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt

-- Ignore all other events
handleEvent _ = return ()


-- Try to safely delete file, catch any exception
tryDeleteFile :: FilePath -> IO (Either SomeException ())
tryDeleteFile path = try (removeFile path)


-- Separate successful and failed removals into 2 distinct lists
partitionRes :: [Either a b] -> ([b], [a])
partitionRes = foldr f ([], [])
  where
    f (Left e) (rs, ls) = (rs, e:ls)
    f (Right r) (rs, ls) = (r:rs, ls)


-- Update the nth group in the list with a new value
updateGroupAt :: Int -> a -> [a] -> [a]
updateGroupAt id newVal xs = take id xs ++ [newVal] ++ drop (id + 1) xs


-- Launches the TUI using Brick
launchUI :: [[FilePath]] -> IO ()
launchUI groups = 
  if null groups
    then putStrLn "No duplicates found"
    else void $ defaultMain app (initState groups)
