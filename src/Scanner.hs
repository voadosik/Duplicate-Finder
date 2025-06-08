module Scanner (findDuplicates) where

import System.Directory
import System.FilePath
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import Control.Exception (try, SomeException)
import Control.Monad (foldM)
import Hasher (rollingHash)

type FileGroup = [FilePath]

groupBySize :: FilePath -> IO (M.Map Integer [FilePath])
groupBySize dir = do
    files <- listDirectoryRecursive dir
    foldM insertFile M.empty files
  where
    insertFile acc fp = do 
        eSize <- try (getFileSize fp) :: IO (Either SomeException Integer)
        case eSize of
            Left _ -> return acc
            Right size -> return $ M.insertWith (++) size [fp] acc

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive path = do
    contents <- try (listDirectory path) :: IO (Either SomeException [FilePath])
    case contents of
        Left _ -> return []
        Right names -> fmap concat . mapM handlePath $ names
  where
    handlePath name = do
        let fullPath = path </> name
        isDir <- doesDirectoryExist fullPath
        if isDir then listDirectoryRecursive fullPath else return [fullPath]

findDuplicates :: FilePath -> IO [FileGroup]
findDuplicates dir = do
    sizeGroups <- groupBySize dir
    concat <$> mapM groupByHash (filter ((>1) . length . snd) $ M.toList sizeGroups)

groupByHash :: (Integer, [FilePath]) -> IO [FileGroup]
groupByHash (_, files) = do
    let hashFile fp = do
            eData <- try (BS.readFile fp) :: IO (Either SomeException BS.ByteString)
            return $ either (const Nothing) (Just . rollingHash) eData
    hashes <- mapM hashFile files
    let pairs = zip files hashes
    return . filter ((>1) . length) . M.elems $
        foldr (\(fp, mhash) acc ->
                case mhash of
                    Just h -> M.insertWith (++) h [fp] acc
                    Nothing -> acc)
              M.empty
              pairs
