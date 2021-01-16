{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Debug.Trace
import GHC
import Data.Foldable
import Data.Monoid
import Data.String
import Data.List (isPrefixOf)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Set        as Set
import System.Environment
import Options.Applicative

import qualified Language.Haskell.GHC.ExactPrint as Exact
import qualified Language.Haskell.GHC.ExactPrint.Types as Exact
import qualified Language.Haskell.GHC.ExactPrint.Parsers as Exact
import qualified Language.Haskell.GHC.ExactPrint.Annotate as Exact
import qualified RdrName
import qualified OccName

import HsExtension
-- deriving instance Show RdrName.RdrName
-- deriving instance Show OccName.OccName


----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

data Haddock = Haddock
  { hdkAnnotation :: Exact.AnnKey
  , hdkHaddock    :: [String]
  }
  deriving Show

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
  }
  where
    comments = dropWhile (not . isPrefixOf "-- |")
               [ c | (Exact.Comment c _ _, _) <- Exact.annPriorComments $ anns ! k ]


parseVectorMod :: FilePath -> IO VecMod
parseVectorMod path = do
  str <- readFile path
  Exact.parseModuleFromString path (stashCPP str) >>= \case
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

copyHaddock :: Set.Set RdrName -> VecMod -> VecMod -> VecMod
copyHaddock touch from to = to
  { vecAnns = appEndo update $ vecAnns to
  }
  where
    update   = fold haddocks
    haddocks = (if null touch then id else flip Map.restrictKeys touch)
             $ Map.intersectionWith copyOneHaddock (vecExpKeys from) (vecExpKeys to)

copyOneHaddock :: Haddock -> Haddock -> Endo Exact.Anns
copyOneHaddock Haddock{hdkHaddock=[]} _ = mempty
copyOneHaddock from to
  | hdkHaddock from == hdkHaddock to = mempty
  | otherwise                        = Endo $ Map.adjust replace (hdkAnnotation to)
  where
    replace a = a { Exact.annPriorComments = repl $ Exact.annPriorComments a }

    repl old = keep ++ merge old (hdkHaddock from)
      where
        -- Number of non-haddock lines
        n = length old - length (hdkHaddock to)
        (keep,replace) = splitAt n old
        --
        merge _ [] = []
        merge ((Exact.Comment _ sp _, dp):olds) (cmt:news)
          = (Exact.Comment cmt sp Nothing, dp)
          : merge olds news
        merge [] (cmt:news)
          = (Exact.Comment cmt (UnhelpfulSpan "") Nothing, Exact.DP (1,0))
          : merge [] news




prefix :: String 
prefix = "../vector/"

pureV :: (String, [String])
pureV =
  ( "Data/Vector/Generic.hs"
  , [ "Data/Vector.hs"
    , "Data/Vector/Storable.hs"
    , "Data/Vector/Unboxed.hs"
    , "Data/Vector/Primitive.hs"
    ]
  )

modPure :: Set.Set RdrName -> IO ()
modPure names = do
  mG <- parseVectorMod $ prefix ++ fst pureV
  forM_ (snd pureV) $ \nm -> do
    mV <- parseVectorMod $ prefix ++ nm
    writeFile (prefix++nm) $ pprVectorMod $ copyHaddock names mG mV

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
    CopyHaddock{..} -> modPure (Set.fromList $ mkRdrName <$> functionNames)
    -- copyHaddock (Set.fromList functionNames) copyPrefix

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
