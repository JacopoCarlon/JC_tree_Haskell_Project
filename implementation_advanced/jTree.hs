{-# LANGUAGE OverloadedStrings #-}

import System.Directory (listDirectory, doesDirectoryExist, makeAbsolute, canonicalizePath)
import System.FilePath
import System.Posix.Files (getFileStatus, fileMode, fileSize, isSymbolicLink, readSymbolicLink, getSymbolicLinkStatus)
import System.Posix.Types (FileMode)
import Options.Applicative
import Control.Monad (forM_, when, filterM)
import Data.List (intercalate, isPrefixOf, sort)
import System.Console.ANSI
import System.Environment (getArgs)
import Data.Maybe (fromMaybe, isJust)
import Data.Bits ((.|.), (.&.))
import Control.Exception (handle, SomeException(..))
import Numeric (showOct)
import Data.Bits (shiftL)
import qualified Data.Set as Set

-- Command-line options
data Options = Options
  { optAll        :: Bool
  , optDirOnly    :: Bool
  , optFollow     :: Bool
  , optFullPath   :: Bool
  , optColor      :: Bool
  , optPermissions:: Bool
  , optSize       :: Bool
  , optLevel      :: Maybe Int
  , optOutput     :: Maybe FilePath
  } deriving Show

-- Parser for command-line options
optionsParser :: Parser Options
optionsParser = Options
  <$> switch (long "a" <> help "Include hidden files")
  <*> switch (long "d" <> help "List directories only")
  <*> switch (long "L" <> help "Follow symbolic links")
  <*> switch (long "f" <> help "Print full path prefix")
  <*> switch (long "c" <> help "Colorize output")
  <*> switch (long "p" <> help "Show permissions")
  <*> switch (long "s" <> help "Show file sizes")
  <*> optional (option auto (long "level" <> short 'L' <> metavar "DEPTH" <> help "Max depth"))
  <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output file"))

-- Main function
main :: IO ()
main = do
  (path, opts) <- execParser $ info (parser <**> helper)
    ( fullDesc
    <> progDesc "Display directory tree structure"
    <> header "haskell-tree - directory tree viewer" )
  
  -- Disable color option if output file is specified
  let adjustedOpts = if isJust (optOutput opts)
                     then opts { optColor = False }
                     else opts
  
  absPath <- handle (\(SomeException _) -> normalise <$> makeAbsolute path) $ do
    if optFullPath adjustedOpts
      then canonicalizePath path
      else normalise <$> makeAbsolute path
  
  let initialDisplayPath = if optFullPath adjustedOpts
                           then path ++ " : " ++ absPath
                           else path
                        
  tree <- generateTree absPath 0 adjustedOpts Set.empty [] initialDisplayPath
  case optOutput adjustedOpts of
    Just outFile -> writeFile outFile (unlines tree)
    Nothing -> forM_ tree putStrLn
  where
    parser = (,) <$> argument str (metavar "DIR" <> value ".") <*> optionsParser

-- Generate directory tree structure
generateTree :: FilePath -> Int -> Options -> Set.Set FilePath -> [Bool] -> String -> IO [String]
generateTree path depth opts visitedPaths isLastStack displayPath = handle handler $ do
  normPath <- if optFollow opts || optFullPath opts
              then handle (\(SomeException _) -> return path) $ canonicalizePath path
              else normalise <$> makeAbsolute path

  let rootLines = if depth == 0 
                  then [displayPath]
                  else []
  
  isLink <- isSymbolicLink <$> getSymbolicLinkStatus path
  if isLink && not (optFollow opts)
    then do
      target <- readSymbolicLink path
      targetPath <- handle (\(SomeException _) -> return target) $ do
        if isAbsolute target
          then canonicalizePath target
          else canonicalizePath (takeDirectory path </> target)
      
      -- Check if target is a directory
      isTargetDir <- handle (\(SomeException _) -> return False) $
        doesDirectoryExist targetPath
            
      let resolvedTarget = if optFullPath opts then targetPath else target
          displayName = takeFileName path
          displayTarget = resolvedTarget
          
          -- Apply colors based on link and target type
          coloredLink = if optColor opts
                        then setSGRCode [SetColor Foreground Dull Yellow] ++ displayName ++ 
                             setSGRCode [Reset] ++ " -> " ++
                             (if isTargetDir 
                              then setSGRCode [SetColor Foreground Dull Blue] 
                              else setSGRCode [Reset]) ++ 
                             displayTarget ++ setSGRCode [Reset]
                        else displayName ++ " -> " ++ displayTarget
      
      targetStatus <- handle (\(SomeException _) -> return False) $ do
        normalizedTarget <- if optFullPath opts
                            then canonicalizePath targetPath
                            else normalise <$> makeAbsolute targetPath
        return $ Set.member normalizedTarget visitedPaths
        
      let cycleInfo = if targetStatus
                      then "  [recursive, not followed]"
                      else ""
          prefix = if depth > 0 then treePrefix isLastStack else ""
          
      return $ rootLines ++ [prefix ++ coloredLink ++ cycleInfo]
    else if Set.member normPath visitedPaths
      then do
        let prefix = fakeTreePrefix isLastStack
        return $ rootLines ++ [prefix ++ " ^^^ [Cycle detected: " ++ normPath ++ "]"]
      else do
        entries <- getEntries path opts
        case entries of
          [] -> return rootLines
          _  -> do
            entryLines <- concat <$> mapM (\(entry, idx) -> 
                                    let isLast = idx == length entries - 1
                                        newIsLastStack = isLastStack ++ [isLast]
                                    in processEntry path depth opts 
                                        (Set.insert normPath visitedPaths)
                                        newIsLastStack
                                        (entry, isLast)) 
                                  (zip entries [0..length entries - 1])
            return $ rootLines ++ entryLines
  where
    handler :: SomeException -> IO [String]
    handler e = return ["Error: " ++ show e]

-- Process individual directory entry
processEntry :: FilePath -> Int -> Options -> Set.Set FilePath -> [Bool] -> (FilePath, Bool) -> IO [String]
processEntry parentPath depth opts visitedPaths isLastStack (entry, isLast) = do
  let fullPath = parentPath </> entry
  
  entryLine <- formatEntry fullPath entry depth isLastStack opts visitedPaths
  
  isDir <- checkIfDirectory fullPath opts
  isLink <- isSymbolicLink <$> getSymbolicLinkStatus fullPath
  
  if isDir && shouldRecurse depth opts && (not isLink || optFollow opts)
    then do
      children <- generateTree fullPath (depth + 1) opts visitedPaths isLastStack entry
      return $ entryLine : children
    else return [entryLine]

-- Format a single entry line
formatEntry :: FilePath -> FilePath -> Int -> [Bool] -> Options -> Set.Set FilePath -> IO String
formatEntry fullPath entry depth isLastStack opts visitedPaths = do
  isLink <- isSymbolicLink <$> getSymbolicLinkStatus fullPath
  isDir <- checkIfDirectory fullPath opts
  
  displayName <- if isLink
                then do
                  target <- readSymbolicLink fullPath
                  targetPath <- handle (\(SomeException _) -> return target) $ do
                    if isAbsolute target
                      then canonicalizePath target
                      else canonicalizePath (takeDirectory fullPath </> target)
                  
                  -- Check if target is a directory
                  isTargetDir <- handle (\(SomeException _) -> return False) $
                    doesDirectoryExist targetPath
                  
                  let resolvedTarget = if optFullPath opts then targetPath else target
                  
                  -- Apply colors for symlinks
                  let coloredLink = if optColor opts
                                    then setSGRCode [SetColor Foreground Dull Yellow] ++ entry ++ 
                                         setSGRCode [Reset] ++ " -> " ++
                                         (if isTargetDir 
                                          then setSGRCode [SetColor Foreground Dull Blue] 
                                          else setSGRCode [Reset]) ++ 
                                         resolvedTarget ++ setSGRCode [Reset]
                                    else entry ++ " -> " ++ resolvedTarget
                  
                  targetStatus <- handle (\(SomeException _) -> return False) $ do
                    normalizedTarget <- if optFullPath opts
                                        then canonicalizePath targetPath
                                        else normalise <$> makeAbsolute targetPath
                    return $ Set.member normalizedTarget visitedPaths
                  
                  let cycleInfo = if targetStatus
                                  then "  [recursive, not followed]"
                                  else ""
                  return $ coloredLink ++ cycleInfo
                else return entry
  
  let styledName = if isLink 
                   then displayName  -- Already styled in the previous block
                   else applyColor isDir displayName opts
      prefix = treePrefix isLastStack
  
  perms <- if optPermissions opts then formatPermissions fullPath else return ""
  size <- if optSize opts then formatSize fullPath else return ""
  
  return $ prefix ++ perms ++ size ++ styledName

-- Tree prefix generation
treePrefix :: [Bool] -> String
treePrefix [] = ""
treePrefix isLastStack = 
  let initParts = [if isLast then "    " else "│   " | isLast <- init isLastStack]
      lastPart = if last isLastStack then "└── " else "├── "
  in concat initParts ++ lastPart

-- fake Tree prefix generation
fakeTreePrefix :: [Bool] -> String
fakeTreePrefix [] = ""
fakeTreePrefix isLastStack = 
  let initParts = [if isLast then "    " else "│   " | isLast <- init isLastStack]
      lastPart = if last isLastStack then "    " else "│   "
  in concat initParts ++ lastPart

-- Directory handling functions
getEntries :: FilePath -> Options -> IO [FilePath]
getEntries path opts = do
  allEntries <- listDirectory path
  filtered <- filterM (\entry -> shouldInclude path entry opts) allEntries
  return $ sort filtered

shouldInclude :: FilePath -> FilePath -> Options -> IO Bool
shouldInclude parentPath entry opts = do
  let fullPath = parentPath </> entry
      isHidden = "." `isPrefixOf` entry
  
  status <- getSymbolicLinkStatus fullPath
  let isSymlink = isSymbolicLink status
  
  isDir <- checkIfDirectory fullPath opts
  
  return $ (optAll opts || not isHidden) &&
           (not (optDirOnly opts) || isDir)

checkIfDirectory :: FilePath -> Options -> IO Bool
checkIfDirectory path opts = do
  symlinkStatus <- handle (\(SomeException _) -> return undefined) $ getSymbolicLinkStatus path
  let isSymlink = isSymbolicLink symlinkStatus
  if isSymlink
    then if optFollow opts
         then do
           targetStatus <- handle (\(SomeException _) -> return undefined) $ getFileStatus path
           return $ isDirectory targetStatus
         else return False
    else doesDirectoryExist path
  where
    isDirectory status = (fileMode status .&. 0o040000) /= 0

-- Helper functions
applyColor :: Bool -> String -> Options -> String
applyColor isDir name opts
  | optColor opts && isDir = setSGRCode [SetColor Foreground Dull Blue] ++ name ++ setSGRCode [Reset]
  | optColor opts          = name
  | otherwise              = name

showFileMode :: FileMode -> String
showFileMode mode = modeChar : concat [permGroup (mode .&. mask) shift | (mask, shift) <- masks]
  where
    modeChar
      | mode .&. 0o040000 /= 0 = 'd'
      | mode .&. 0o120000 /= 0 = 'l'
      | otherwise              = '-' 
    masks = [(0o700, 6), (0o070, 3), (0o007, 0)]
    permGroup m s = [
      if m .&. (4 `shiftL` s) /= 0 then 'r' else '-',
      if m .&. (2 `shiftL` s) /= 0 then 'w' else '-',
      if m .&. (1 `shiftL` s) /= 0 then 'x' else '-']

formatPermissions :: FilePath -> IO String
formatPermissions path = do
  status <- getFileStatus path
  return $ " [" ++ showFileMode (fileMode status) ++ "]"

formatSize :: FilePath -> IO String
formatSize path = do
  status <- getFileStatus path
  return $ " " ++ show (fileSize status `div` 1024) ++ "KB "

shouldRecurse :: Int -> Options -> Bool
shouldRecurse depth opts = case optLevel opts of
  Just maxDepth -> depth < maxDepth
  Nothing       -> True

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat (mapM f xs)