{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import GHC
import Data.Foldable
import Data.Monoid
import Data.String
import Data.Maybe
import Data.List       (isPrefixOf)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Options.Applicative
import System.FilePath ((</>))
import Text.Regex.Applicative

import qualified Language.Haskell.GHC.ExactPrint         as Exact
import qualified Language.Haskell.GHC.ExactPrint.Types   as Exact
import qualified Language.Haskell.GHC.ExactPrint.Parsers as Exact
import qualified RdrName
import qualified OccName


----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

-- | Split haddocks into hunk of normal documentation and doctests
data HaddockHunk
  = NormalHaddock [String]
  | Doctest       [String]
  deriving (Show,Eq)

-- tst =
--   [ "-- | /O(n)/ Yield the minimum element of the vector by comparing the results",
--     "-- of a key function on each element. In case of a tie, the first occurrence",
--     "-- wins. The vector may not be empty.",
--     "--",
--     "-- >>> import qualified Data.Vector as V",
--     "-- >>> V.minimumOn fst $ V.fromList [(1.0,'a'), (1.0,'b')]",
--     "-- (1.0,'a')",
--     "--",
--     "-- >>> ddd"
--   ]

splitHunks :: [String] -> [HaddockHunk]
splitHunks hdk
  = fromMaybe (error $ unlines $ "splitHunks: impossible" : hdk)
  $ match (skip *> (chainN <|> pure [])) hdk
  where
    emptyline s = Nothing == match ("--" <* many (sym ' ')) s
    skip    = many $ psym $ not . ("-- |" `isPrefixOf`)
    normal  = fmap ((:[]) . NormalHaddock)
            $ some $ psym $ not . ("-- >>>" `isPrefixOf`)
    doctest = fmap ((:[]) . Doctest . concat)
            $ some $ do x <- some $ psym ("-- >>>" `isPrefixOf`)
                        y <- many $ psym $ not . emptyline
                        z <- ((:[]) <$> psym emptyline) <|> pure []
                        pure $ x ++ y ++ z
    chainN = liftA2 (++) normal  (chainD <|> pure [])
    chainD = liftA2 (++) doctest (chainN <|> pure [])



data Haddock = Haddock
  { hdkAnnotation :: Exact.AnnKey
  , hdkHaddock    :: [HaddockHunk]
  , hdkPragma     :: [Pragmas]
  }
  deriving Show

data Pragmas
  = NoDoctests [Vec]
  deriving Show

stripDoctest :: [HaddockHunk] -> [HaddockHunk]
stripDoctest xs = [ x | x@NormalHaddock{} <- xs ]

unpackHunks :: [HaddockHunk] -> [String]
unpackHunks = concatMap $ \case
  NormalHaddock x -> x
  Doctest       x -> x



data VecMod = VecMod
  { vecAnns    :: Exact.Anns      -- ^ Exactprint annotations
  , vecAST     :: ParsedSource    -- ^ Parsed AST
  , vecExpKeys :: Map.Map RdrName Haddock
    -- ^ Information about haddocks attached to the top level definitions
  }

stashCPP :: String -> String
stashCPP = unlines . map stash . lines
  where
    stash ('#':s) = '-':'-':' ':'#':s
    stash s       = s

unstashCPP :: String -> String
unstashCPP = unlines . map unstash . lines
  where
    unstash ('-':'-':' ':'#':s) = '#':s
    unstash s                   = s

makeHaddock :: Exact.Anns -> Exact.AnnKey -> Haddock
makeHaddock anns k = Haddock
  { hdkAnnotation = k
  , hdkHaddock    = comments
  , hdkPragma     = parsePragma =<< Exact.annPriorComments (anns ! k)
  }
  where
    comments = splitHunks [ c | (Exact.Comment c _ _, _) <- Exact.annPriorComments $ anns ! k ]
    parsePragma (Exact.Comment s _ _,_)
      | Just vec <- match reNoDoctest s = [NoDoctests vec]
      | otherwise                       = []
    reNoDoctest = "--" *> many " " *> "NO_DOCTEST:" *> many " " *> many vec
      where
        vec = asum [ VecV <$ "V"
                   , VecP <$ "P"
                   , VecU <$ "U"
                   , VecS <$ "S"
                   ] <* many " "

parseVectorMod :: FilePath -> IO VecMod
parseVectorMod path = do
  hsStr <- readFile path
  Exact.parseModuleFromString path (stashCPP hsStr) >>= \case
    Left  err -> do
      mapM_ print err
      error "Cannot proceed"
    Right (anns,ast) -> do
      let L _ (HsModule{ hsmodDecls   = decls
                       , hsmodExports = Just (L _ exports)
                       }
              ) = ast
      let exported = Set.fromList
                   $ [ nm | L _ (IEVar _ (L _ (IEName (L _ nm)))) <- exports ]
          keys     = Map.fromList
                   $ [ ( nm
                       , makeHaddock anns $ Exact.AnnKey loc (Exact.CN "TypeSig")
                       )
                     | (L loc (SigD NoExt (TypeSig NoExt [L _ nm] _))) <- decls
                     , nm `Set.member` exported
                     ]
      return $ VecMod
        { vecAnns    = anns
        , vecAST     = ast
        , vecExpKeys = keys
        }

pprVectorMod :: VecMod -> String
pprVectorMod m = unstashCPP $ Exact.exactPrint (vecAST m) (vecAnns m)

----------------------------------------------------------------
-- Adjust comments
----------------------------------------------------------------

data Vec
  = VecV
  | VecP
  | VecU
  | VecS
  | VecG
  deriving (Show,Eq,Enum,Bounded)

vecPath :: Vec -> FilePath
vecPath = \case
  VecV -> "Data/Vector.hs"
  VecS -> "Data/Vector/Storable.hs"
  VecU -> "Data/Vector/Unboxed.hs"
  VecP -> "Data/Vector/Primitive.hs"
  VecG -> "Data/Vector/Generic.hs"

vecImport :: Vec -> String
vecImport = \case
  VecV -> "-- >>> import qualified Data.Vector as V"
  VecU -> "-- >>> import qualified Data.Vector.Unboxed as U"
  VecP -> "-- >>> import qualified Data.Vector.Primitive as P"
  VecG -> "-- >>> import qualified Data.Vector.Generic as G"
  VecS -> "-- >>> import qualified Data.Vector.Storable as S"

vecAlias :: Vec -> Char
vecAlias = \case
  VecV -> 'V'
  VecP -> 'P'
  VecU -> 'U'
  VecS -> 'S'
  VecG -> 'G'

targets :: [Vec]
targets = [VecV, VecU, VecP, VecS]

copyHaddock :: Vec -> Set.Set RdrName -> VecMod -> VecMod -> VecMod
copyHaddock vec touch from to = to
  { vecAnns = appEndo update $ vecAnns to
  }
  where
    update   = fold haddocks
    haddocks = (if null touch then id else flip Map.restrictKeys touch)
             $ Map.intersectionWith (copyOneHaddock vec) (vecExpKeys from) (vecExpKeys to)

copyOneHaddock :: Vec -> Haddock -> Haddock -> Endo Exact.Anns
copyOneHaddock _ Haddock{hdkHaddock=[]} _ = mempty
copyOneHaddock vec from to
  | hdkHaddock from == hdkHaddock to = mempty
  | otherwise                        = Endo $ Map.adjust replaceCmt (hdkAnnotation to)
  where
    replaceCmt a = a { Exact.annPriorComments = repl $ Exact.annPriorComments a }

    repl old = keep ++ merge old new
      where
        -- Number of non-haddock lines
        n    = length old - length (unpackHunks $ hdkHaddock to)
        keep = take n old
        --
        merge _ [] = []
        merge ((Exact.Comment _ sp _, dp):olds) (cmt:news)
          = (Exact.Comment cmt sp Nothing, dp)
          : merge olds news
        merge [] (cmt:news)
          = (Exact.Comment cmt (UnhelpfulSpan "") Nothing, Exact.DP (1,0))
          : merge [] news
        -- Fixup for doctests
        droppedDoctests = concat [ t | NoDoctests t <- hdkPragma from ]
        new = map fixup
            $ unpackHunks
            $ (if vec `elem` droppedDoctests then stripDoctest else id)
            $ hdkHaddock from
        fixup s
          | "-- >>>" `isPrefixOf` s = replaceQualifiers s
          | s == vecImport VecV     = vecImport vec
          | otherwise               = s
        replaceQualifiers (' ':'V':'.':s) = ' ':vecAlias vec:'.': replaceQualifiers s
        replaceQualifiers (c:s)           = c : replaceQualifiers s
        replaceQualifiers []              = []


modPure :: FilePath -> Set.Set RdrName -> IO ()
modPure prefix names = do
  mG <- parseVectorMod $ prefix </> vecPath VecG
  forM_ targets $ \vec -> do
    mV <- parseVectorMod $ prefix </> vecPath vec
    writeFile (prefix </> vecPath vec) $! pprVectorMod $ copyHaddock vec names mG mV

mkRdrName :: String -> RdrName
mkRdrName = RdrName.mkUnqual OccName.varName . fromString





----------------------------------------------------------------
-- Command line parser
----------------------------------------------------------------

main :: IO ()
main = do
  cmd <- customExecParser (prefs showHelpOnError)
       $ info (helper <*> parserCLI)
         (  fullDesc
         <> header   "Program for working with keys for coin node"
         <> progDesc ""
         )
  case cmd of
    CopyHaddock{..} -> modPure copyPrefix (Set.fromList $ mkRdrName <$> functionNames)

data Cmd
  = CopyHaddock
    { copyPrefix    :: FilePath
    , functionNames :: [String]
    }
  deriving (Show)

parserCLI :: Parser Cmd
parserCLI = subparser
  ( command "copy"  (parserCopyPrefix  `info` header "Copy haddocks")
  )

parserCopyPrefix :: Parser Cmd
parserCopyPrefix = helper <*> do
  copyPrefix <- strOption ( short   'v'
                         <> long    "vector"
                         <> help    "path to vector's source"
                         <> metavar "DIR"
                         <> value   "."
                          )
  functionNames <- many $ strArgument ( help    "Function to copy from"
                                     <> metavar "FUN"
                                      )
  pure CopyHaddock{..}
