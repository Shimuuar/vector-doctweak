{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Foo where

import Debug.Trace
import GHC
import Data.Foldable
import Data.Monoid
import Data.List (isPrefixOf)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Set        as Set
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

copyHaddock :: VecMod -> VecMod -> VecMod
copyHaddock from to = to
  { vecAnns = appEndo update $ vecAnns to
  }
  where
    update   = fold haddocks
    haddocks = Map.intersectionWith copyOneHaddock (vecExpKeys from) (vecExpKeys to)

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



    -- new = replicate (
    --
    -- nTo = length $

    -- repl = add
    --      . reverse
    --      . drop (length $ hdkHaddock to)
    --      . (\x -> trace (("===\n" ++) $ unlines $ map show $ take (length $ hdkHaddock to) x) x)
    --      . reverse
    -- add c = c <> [ ( Exact.Comment str (UnhelpfulSpan "") Nothing
    --                , Exact.DP (1, 0)
    --                )
    --              | str <- hdkHaddock from
    --              ]
-- copyOneHaddock Haddock{..} = EndoMap.adjust undefined hdk


test :: IO ()
test = do
  mG <- parseVectorMod "Data/Vector/Generic.hs"
  mV <- parseVectorMod "Data/Vector.hs"
  let mV' = copyHaddock mG mV
  writeFile "Data/Vector.hs" $ pprVectorMod mV'
  -- mapM_ print $ vecExpKeys m
  return ()
{-
test :: IO ()
test = do
  r <- Exact.parseModule "Data/Vector/Generic.hs"
  case r of
    Left  err -> mapM_ print err
    Right (anns,ast) -> do
      let ppr :: Exact.Annotate ast => Located ast -> String
          ppr x = Exact.exactPrint x anns
      --
      let L _ (HsModule{ hsmodDecls   = decls
                       , hsmodExports = Just (L _ exports)
                       }
              ) = ast
      -- Build list of values export from module
      let exported = Set.fromList
                   $ [ nm | L _ (IEVar _ (L _ (IEName (L _ nm)))) <- exports ]
      -- Build list of haddocks
      -- let pp (s,d,cmts) = do
      --       putStrLn s
      --       print d
      --       mapM_ (print . fst) cmts
      let defs = [ ( Exact.AnnKey loc (Exact.CN "TypeSig")
                   -- , anns ! Exact.AnnKey loc (Exact.CN "TypeSig")
                   )
                 | d@(L loc (SigD NoExt (TypeSig NoExt [i@(L _ nm)] _))) <- decls
                 , nm `Set.member` exported
                 ]

      -- mapM_ print [ anns ! a | a <- defs ]
      let extraC = Exact.Comment "-- XXX" (UnhelpfulSpan "") Nothing
      -- Append to every comment
      let inc (Exact.DP (y,x)) = Exact.DP (y+1,x)
          append a = a { Exact.annPriorComments =
                           Exact.annPriorComments a <>
                           [( extraC
                            , Exact.DP (1, 0))]
                       -- , Exact.annEntryDelta    = inc $ Exact.annEntryDelta a
                       }
          -- modif k = Map.modify k
      let anns' = appEndo (foldMap (\k -> Endo $ Map.adjust append k) defs) anns
      putStrLn $ unlines $ take 100 $ drop 200 $ lines $  Exact.exactPrint ast anns'
      return ()
      -- mapM_ print $ Map.keys anns
      -- return ()
      -- mapM_ (print) $ catMaybes
      --   [ case bind of
      --       FunBind{fun_id = nm} -> Just ("FunBind", ppr nm)
      --       -- PatBind{}            -> Just ("PatBind","")
      --       -- VarBind{}            -> Just ("VarBind","")
      --       -- AbsBinds{}           -> Just ("AbsBinds","")
      --       _                    -> Nothing
      --   | L _ (ValD v bind) <- decls
      --   ]
      -- mapM_ print
      --   [ case dd of
      --       DocCommentNext s -> ("DocCommentNext", unpackHDS s)
      --       DocCommentPrev s -> ("DocCommentPrev", unpackHDS s)
      --       DocCommentNamed nm s -> ("DocCommentNamed: " ++ nm, unpackHDS s)
      --       DocGroup n s -> ("DocGroup: " ++ show n, unpackHDS s)
      --   | L _ (DocD _ dd) <- decls
      --   ]
-- mapM_ print
--         [ case dd of
--             DocCommentNext s -> ("DocCommentNext", unpackHDS s)
--             DocCommentPrev s -> ("DocCommentPrev", unpackHDS s)
--             DocCommentNamed nm s -> ("DocCommentNamed: " ++ nm, unpackHDS s)
--             DocGroup n s -> ("DocGroup: " ++ show n, unpackHDS s)
--         | L _ (DocD _ dd) <- decls
--         ]
            --
      -- print $ Map.fromListWith (+) $ map (\x -> (x,1::Int))
      --   [ case d of
      --       TyClD{} -> "TyClD"
      --       InstD{} -> "InstD"
      --       DerivD{} -> "DerivD"
      --       ValD{} -> "ValD"
      --       SigD{} -> "SigD"
      --       -- KindSig{} -> "KindSigD"
      --       DefD{} -> "DefD"
      --       ForD{} -> "ForD"
      --       WarningD{} -> "WarningD"
      --       AnnD{} -> "AnnD"
      --       RuleD{} -> "RuleD"
      --       SpliceD{} -> "SpliceD"
      --       DocD{} -> "DocD"
      --       RoleAnnotD{} -> "RoleAnnotD"
      --       XHsDecl{} -> "XHsDecl"
      --   | L _ d <- decls
      --   ]
      -- return ()



      -- import GHC
-- import GHC.Paths ( libdir )
-- import Outputable
-- import qualified ErrUtils
-- import qualified Bag

-- -- deriving instance Show ErrUtils.ErrorMessages
-- -- instance Show a => Show (Bag.Bag a) w

-- test :: IO ()
-- test = do
--   dyn <- runGhc (Just libdir) $ getSessionDynFlags
--   txt <- readFile "Data/Vector/Generic.hs"
--   -- let (_, Right hsMod) = parser txt dyn "@"
--   let (_, Left err) = parser txt dyn "@"
--   mapM_ print err
--   --     L _ (HsModule{ hsmodDecls = decls }) = hsMod
--   -- return ()
--   -- mapM_ print [ ()
--   --             | L _ (ValD v bind) <- decls
--   --             ]
--   -- runGhc (Just libdir) $ getSessionDynFlags
--   -- env    <- getSession
--   -- dflags <-

--   -- target <- guessTarget "Data/Vector/Generic.hs" Nothing
--   -- setTargets [target]
--   -- load LoadAllTargets

--   return ()
-}
