module Scanner (findDups) where

import System.Directory ( doesDirectoryExist, getFileSize, listDirectory )
import System.FilePath ( (</>) )
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import Control.Exception (try, SomeException)
import Control.Monad (foldM)
import Hasher (rollingHash)

type FileGroup = [FilePath]

-- Group files by their sizes
-- Map size -> [file of that size]
groupBySize :: FilePath -> IO (M.Map Integer FileGroup)
groupBySize dir = do
    files <- listDir dir -- get all files inside directory
    foldM insertFile M.empty files 
  where
    insertFile a fp = do 
        -- try to safely get file size, if exception, returns Left
        s <- try (getFileSize fp) :: IO (Either SomeException Integer)
        case s of
            Left _ -> return a
            -- if Right insert into a Map, appending if the size key exists
            Right size -> return $ M.insertWith (++) size [fp] a


-- Recursively list all file paths inside a given directory
-- Empty list if something went wrong
listDir :: FilePath -> IO FileGroup
listDir path = do
    -- if error, returns []
    contents <- try (listDirectory path) :: IO (Either SomeException FileGroup)
    case contents of
        Left _ -> return []
        -- recursively handle subdirectories
        Right names -> concat <$> mapM handlePath names
    where
        -- either directory or a final filepath
        handlePath name = do
            let fullPath = path </> name
            isDir <- doesDirectoryExist fullPath
            if isDir then listDir fullPath else return [fullPath]


-- Find groups of duplicates in directory
-- Group is determined by equal size and equal hash
findDups :: FilePath -> IO [FileGroup]
findDups dir = do
    sizeGroups <- groupBySize dir
    -- filter file soze groups with one file
    -- for each group find duplicates using rolling hash
    concat <$> mapM groupByHash (filter ((>1) . length . snd) $ M.toList sizeGroups)

-- Group files by their hash values
groupByHash :: (Integer, FileGroup) -> IO [FileGroup]
groupByHash (_, files) = do
    -- safely read file into ByteString, if reading failed return Nothing, otherwise Just
    let hashFile fp = do
            d <- try (BS.readFile fp) :: IO (Either SomeException BS.ByteString)
            return $ either (const Nothing) (Just . rollingHash) d
    hashes <- mapM hashFile files
    let pairs = zip files hashes
    return . filter ((>1) . length) . M.elems $
    -- build Map hash -> [list of files], skipping those that can't be read, if is Nothing
        foldr (\(fp, mh) a ->
                case mh of
                    Just h -> M.insertWith (++) h [fp] a
                    Nothing -> a)
            M.empty
            pairs
