{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}

import qualified Control.DeepSeq as DeepSeq
import qualified Criterion.Main as Criterion
import qualified Data.Maybe as Maybe
import qualified "semver" Data.SemVer as SemVer
import qualified "semver-range" Data.SemVer as SemVerRange
import qualified Data.Version as Version
import qualified GHC.Generics as Ghc
import qualified Salve.Internal as Salve
import qualified Text.ParserCombinators.ReadP as ReadP

deriving instance DeepSeq.NFData Salve.Build
deriving instance DeepSeq.NFData Salve.Constraint
deriving instance DeepSeq.NFData Salve.Operator
deriving instance DeepSeq.NFData Salve.PreRelease
deriving instance DeepSeq.NFData Salve.Version
deriving instance DeepSeq.NFData SemVerRange.PrereleaseTag
deriving instance DeepSeq.NFData SemVerRange.PrereleaseTags
deriving instance DeepSeq.NFData SemVerRange.SemVer
deriving instance DeepSeq.NFData SemVerRange.SemVerRange
deriving instance Ghc.Generic Salve.Build
deriving instance Ghc.Generic Salve.Constraint
deriving instance Ghc.Generic Salve.Operator
deriving instance Ghc.Generic Salve.PreRelease
deriving instance Ghc.Generic Salve.Version
deriving instance Ghc.Generic SemVerRange.SemVerRange

main :: IO ()
main = Criterion.defaultMain
  [ Criterion.bgroup "version"
    [ Criterion.bgroup "parse"
      [ Criterion.bgroup "simple"
        [ Criterion.bench "base" $ Criterion.nf (runReadP Version.parseVersion) "0.0.0"
        , Criterion.bench "salve" $ Criterion.nf Salve.parseVersion "0.0.0"
        , Criterion.bench "semver" $ Criterion.nf (fromRight . SemVer.fromText) "0.0.0"
        , Criterion.bench "semver-range" $ Criterion.nf (fromRight . SemVerRange.parseSemVer) "0.0.0"
        ]
      , Criterion.bgroup "complex"
        [ Criterion.bench "base" $ Criterion.nf (runReadP Version.parseVersion) "1.22.333-pre-build"
        , Criterion.bench "salve" $ Criterion.nf Salve.parseVersion "1.22.333-pre+build"
        , Criterion.bench "semver" $ Criterion.nf (fromRight . SemVer.fromText) "1.22.333-pre+build"
        , Criterion.bench "semver-range" $ Criterion.nf (fromRight . SemVerRange.parseSemVer) "1.22.333-pre+build"
        ]
      ]
    , Criterion.bgroup "render"
      [ Criterion.bgroup "simple"
        [ let Just v = runReadP Version.parseVersion "0.0.0" in Criterion.bench "base" $ Criterion.nf Version.showVersion v
        , let v = Salve.unsafeParseVersion "0.0.0" in Criterion.bench "salve" $ Criterion.nf Salve.renderVersion v
        , let Right v = SemVer.fromText "0.0.0" in Criterion.bench "semver" $ Criterion.nf SemVer.toText v
        , let Right v = SemVerRange.parseSemVer "0.0.0" in Criterion.bench "semver-range" $ Criterion.nf SemVerRange.renderSV v
        ]
      , Criterion.bgroup "complex"
        [ let Just v = runReadP Version.parseVersion "1.22.333-pre-build" in Criterion.bench "base" $ Criterion.nf Version.showVersion v
        , let v = Salve.unsafeParseVersion "1.22.333-pre+build" in Criterion.bench "salve" $ Criterion.nf Salve.renderVersion v
        , let Right v = SemVer.fromText "1.22.333-pre+build" in Criterion.bench "semver" $ Criterion.nf SemVer.toText v
        , let Right v = SemVerRange.parseSemVer "1.22.333-pre+build" in Criterion.bench "semver-range" $ Criterion.nf SemVerRange.renderSV v
        ]
      ]
    ]
  , Criterion.bgroup "constraint"
    [ Criterion.bgroup "parse"
      [ Criterion.bgroup "simple"
        [ Criterion.bench "salve" $ Criterion.nf Salve.parseConstraint "0.0.0"
        , Criterion.bench "semver-range" $ Criterion.nf (fromRight . SemVerRange.parseSemVerRange) "0.0.0"
        ]
      , Criterion.bgroup "complex"
        [ Criterion.bench "salve" $ Criterion.nf Salve.parseConstraint "<1.2.0 <=1.2.1 || =1.2.2 >=1.2.3 || >1.2.4 1.2.5 || 1.2.6 - 1.2.7 ~1.2.8 || ^1.2.9"
        , Criterion.bench "semver-range" $ Criterion.nf (fromRight . SemVerRange.parseSemVerRange) "<1.2.0 <=1.2.1 || =1.2.2 >=1.2.3 || >1.2.4 1.2.5 || 1.2.6 - 1.2.7 ~1.2.8 || ^1.2.9"
        ]
      ]
    , Criterion.bgroup "render"
      [ Criterion.bgroup "simple"
        [ let c = Salve.unsafeParseConstraint "0.0.0" in Criterion.bench "salve" $ Criterion.nf Salve.renderConstraint c
        ]
      , Criterion.bgroup "complex"
        [ let c = Salve.unsafeParseConstraint "<1.2.0 <=1.2.1 || =1.2.2 >=1.2.3 || >1.2.4 1.2.5 || 1.2.6 - 1.2.7 ~1.2.8 || ^1.2.9" in Criterion.bench "salve" $ Criterion.nf Salve.renderConstraint c
        ]
      ]
    ]
  ]

fromRight :: Either a b -> Maybe b
fromRight e = case e of
  Left _ -> Nothing
  Right r -> Just r

runReadP :: ReadP.ReadP a -> String -> Maybe a
runReadP parser string = Maybe.listToMaybe (do
  (x, "") <- ReadP.readP_to_S parser string
  pure x)
