module Scanner (findDups) where

import System.Directory
    ( doesDirectoryExist, getFileSize, listDirectory )
import System.FilePath ( (</>) )
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import Control.Exception (try, SomeException)
import Control.Monad (foldM)
import Hasher (rollingHash)

type FileGroup = [FilePath]

groupBySize :: FilePath -> IO (M.Map Integer FileGroup)
groupBySize dir = do
    files <- listDir dir
    foldM insertFile M.empty files
  where
    insertFile a fp = do 
        s <- try (getFileSize fp) :: IO (Either SomeException Integer)
        case s of
            Left _ -> return a
            Right size -> return $ M.insertWith (++) size [fp] a

listDir :: FilePath -> IO FileGroup
listDir path = do
    contents <- try (listDirectory path) :: IO (Either SomeException FileGroup)
    case contents of
        Left _ -> return []
        Right names -> fmap concat . mapM handlePath $ names
  where
    handlePath name = do
        let fullPath = path </> name
        isDir <- doesDirectoryExist fullPath
        if isDir then listDir fullPath else return [fullPath]

findDups :: FilePath -> IO [FileGroup]
findDups dir = do
    sizeGroups <- groupBySize dir
    concat <$> mapM groupByHash (filter ((>1) . length . snd) $ M.toList sizeGroups)

groupByHash :: (Integer, FileGroup) -> IO [FileGroup]
groupByHash (_, files) = do
    let hashFile fp = do
            d <- try (BS.readFile fp) :: IO (Either SomeException BS.ByteString)
            return $ either (const Nothing) (Just . rollingHash) d
    hashes <- mapM hashFile files
    let pairs = zip files hashes
    return . filter ((>1) . length) . M.elems $
        foldr (\(fp, mh) a ->
                case mh of
                    Just h -> M.insertWith (++) h [fp] a
                    Nothing -> a)
              M.empty
              pairs
