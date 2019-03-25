module MockDrive where

-- import Control.Concurrent
-- threadDelay 1000000

import Control.Monad.State

type Name = String
type MimeType = String
type Id = String
-- data State s a = State { runState :: s -> (a, s)}
newtype File = File Name deriving (Eq, Show)

data Folder = Directory { getDirName :: Name
                        , getDirFiles :: [File]
                        , getDirFolders :: [Folder] } deriving Eq

con :: Name -> Name -> Name
con = (++)
branch :: Name -> Name
branch a = "  |" `con` "\n  --" `con` a `con` "\n"

indentation :: Int -> String
indentation 0 = ""
indentation n = indentation (n - 1) ++ "  "

indentBranch :: Int -> Name -> Name
indentBranch indent a = concat [indentation indent, "|", "\n", indentation indent, "--", a, "\n"]

instance Show Folder where
  show dir = go dir 0 where
    go dir indent =
        case dir of
          Directory name fLs dLs -> indentBranch indent ("Directory " `con` name `con` "\n")
                                    `con` concat (fmap ((indentBranch (indent + 1)) . show) fLs)
                                    `con` concat (fmap (\dir -> go dir (indent + 1)) dLs)

initialState :: Folder -- State Folder Folder
initialState = Directory "root" [] [] -- State $ \folder -> (Dire root, folder)

biggerDir :: Folder
biggerDir = Directory
              "root2"
              [File "a", File "b"]
              [Directory
                "root3"
                [File "a", File "b"]
                []]

biggestDir :: Folder
biggestDir = Directory
              "root2"
              [File "a", File "b"]
              [Directory
                "root3"
                [File "a", File "b"]
                [],
               Directory
                "root4"
                [File "a", File "c"]
                [Directory
                  "root5"
                  [File "d"]
                  []]]

type Path = [Name]
-- define show
addFileRoot :: File -> Folder -> Folder
addFileRoot file dir =
  Directory (getDirName dir)
            (file : getDirFiles dir)
            (getDirFolders dir)

addFolderRoot :: Folder -> Folder -> Folder
addFolderRoot folder dir =
  Directory (getDirName dir)
            (getDirFiles dir)
            (folder : getDirFolders dir)

modFolder :: (Folder -> Folder) -> Path -> Folder -> Folder
modFolder f [] folder = folder
modFolder f (x:xs) folder
  | x == folderName =
    case xs of
      [] -> f folder
      xss -> Directory folderName fileList (fmap (modFolder f xs) folderList)
  | otherwise = folder
  where (folderName,
         fileList,
         folderList) = (getDirName folder,
                        getDirFiles folder,
                        getDirFolders folder)

type Local = State Folder Folder

data Target = Target { getTargetName :: Name,
                       getTargetMime :: MimeType,
                       getTargetId :: Id,
                       getParentId :: Id} deriving (Eq, Show)

mockFilesDb :: [Target]
mockFilesDb = [
  Target { getTargetName = "Root",
           getTargetMime = "Folder",
           getTargetId = "A1",
           getParentId = "NOOOO" },
  Target { getTargetName = "UserFiles",
           getTargetMime = "Folder",
           getTargetId = "B1",
           getParentId = "A1" },
  Target { getTargetName = "hello.Docx",
           getTargetMime = "Doc",
           getTargetId = "A2",
           getParentId = "A1" },
  Target { getTargetName = "details.xlsx",
           getTargetMime = "Spreadsheet",
           getTargetId = "A3",
           getParentId = "A1" },
  Target { getTargetName = "DataFiles",
           getTargetMime = "Folder",
           getTargetId = "C1",
           getParentId = "B1" },
  Target { getTargetName = "log.xlsx",
           getTargetMime = "Spreadsheet",
           getTargetId = "C2",
           getParentId = "C1" }]

fileDownload :: Id -> Path -> Local
fileDownload ident path = do
  originalDirectory <- get
  let file :: [Target]
      file = filter (\target -> getTargetId target == ident) mockFilesDb
  case file of
    [] -> state $ \_ -> (originalDirectory, originalDirectory)
    (x:xs) -> do
      let fileLocal :: File
          fileLocal = File $ getTargetName x
          stateModFunc :: Folder -> Folder
          stateModFunc = modFolder (\rootFolder -> (addFileRoot fileLocal rootFolder))
                                   path
      state $ \folder -> (folder, stateModFunc folder)

folderFrom :: Name -> Path -> Local
folderFrom name path = do
  originalDirectory <- get
  let emptyDir :: Folder
      emptyDir = Directory name [] []
      stateModFunc :: Folder -> Folder
      stateModFunc = modFolder (\rootFolder -> (addFolderRoot emptyDir rootFolder))
                                               path
  state $ \folder -> (folder, stateModFunc folder)

getFolderList :: Id -> [Target]
getFolderList ident =
  let files = filter (\target -> getParentId target == ident) mockFilesDb
  in case files of
      [] -> []
      xs -> xs

seqMap :: (a -> Local) -> [a] -> Local
seqMap _ [] = get
seqMap f (x:xs) = f x >> seqMap f xs

mimeCheck :: [MimeType] -> MimeType -> Maybe MimeType
mimeCheck [] _ = Nothing
mimeCheck (mimeType : mimeTypes) sampleMimeType
  | mimeType == sampleMimeType = Just mimeType
  | otherwise = mimeCheck mimeTypes sampleMimeType

getFile :: Target -> Path -> Local
getFile target = fileDownload $ getTargetId target

mkFolder :: Target -> Path -> Local
mkFolder target = folderFrom $ getTargetName target

listFolder :: Target -> [Target]
listFolder target = getFolderList $ getTargetId target

getFolder :: Target -> Path -> Local
getFolder target [] = get
getFolder target path@(x:xs) = do
  originalState <- get
  let targetMime :: MimeType
      targetMime = getTargetMime target
  let mimes :: [MimeType]
      mimes = ["Folder","Doc", "Spreadsheet"]
  case mimeCheck mimes targetMime of
    Just "Folder" -> do
      mkFolder target path
      let folderName = getTargetName target
      let subFiles = listFolder target
      case subFiles of
        [] -> get
        targets -> seqMap handle targets
          where handle :: Target -> Local
                handle target
                  | targetMime == "Spreadsheet" ||
                    targetMime == "Doc" = getFile target (path ++ [folderName])
                  | targetMime == "Folder" = getFolder target (path ++ [folderName])
                  where targetMime = getTargetMime target

    _ -> state $ \_ -> (originalState, originalState)

targetedFolder :: Target
targetedFolder = Target { getTargetName = "Root",
                          getTargetMime = "Folder",
                          getTargetId = "A1",
                          getParentId = "" }

newFolder = addFolderRoot biggerDir initialState

flowFromDb :: Local
flowFromDb = getFolder targetedFolder ["root"]

main :: Folder
main =
  execState flowFromDb newFolder
