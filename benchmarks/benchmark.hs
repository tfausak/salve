{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

import qualified Control.DeepSeq as DeepSeq
import qualified Criterion.Main as Criterion
import qualified Data.Maybe as Maybe
import qualified Data.Version as Version
import qualified GHC.Generics as Ghc
import qualified Salve.Internal as Salve
import qualified Text.ParserCombinators.ReadP as ReadP

deriving instance Ghc.Generic Salve.Build
deriving instance Ghc.Generic Salve.Constraint
deriving instance Ghc.Generic Salve.Operator
deriving instance Ghc.Generic Salve.PreRelease
deriving instance Ghc.Generic Salve.Version

deriving instance DeepSeq.NFData Salve.Build
deriving instance DeepSeq.NFData Salve.Constraint
deriving instance DeepSeq.NFData Salve.Operator
deriving instance DeepSeq.NFData Salve.PreRelease
deriving instance DeepSeq.NFData Salve.Version

main :: IO ()
main = Criterion.defaultMain
  [ Criterion.bgroup "Data.Version.parseVersion" (map
    (\ x -> Criterion.bench x (Criterion.nf (runReadP Version.parseVersion) x))
    [ "0.0.0"
    , "123.456.789-pre-release-build-metadata"
    ])
  , Criterion.bgroup "parseVersion" (map
    (\ x -> Criterion.bench x (Criterion.nf Salve.parseVersion x))
    [ "0.0.0"
    , "123.456.789-pre.release+build.metadata"
    ])
  , Criterion.bgroup "parseConstraint" (map
    (\ x -> Criterion.bench x (Criterion.nf Salve.parseConstraint x))
    [ ">=0.0.0"
    , ">1.2.3 || =1.2.3 >=1.2.3 <1.2.3 || 1.2.3 <=1.2.3 ~1.2.3 ^1.2.3 1.2.3 - 1.2.3"
    ])
  , Criterion.bgroup "Data.Version.showVersion" (map
    (\ x ->
      let y = Maybe.fromJust (runReadP Version.parseVersion x)
      in Criterion.bench x (Criterion.nf Version.showVersion y))
    [ "0.0.0"
    , "123.456.789-pre-release-build-metadata"
    ])
  , Criterion.bgroup "renderVersion" (map
    (\ x ->
      let y = Salve.unsafeParseVersion x
      in Criterion.bench x (Criterion.nf Salve.renderVersion y))
    [ "0.0.0"
    , "123.456.789-pre.release+build.metadata"
    ])
  , Criterion.bgroup "renderConstraint" (map
    (\ x ->
      let y = Salve.unsafeParseConstraint x
      in Criterion.bench x (Criterion.nf Salve.renderConstraint y))
    [ ">=0.0.0"
    , ">1.2.3 || =1.2.3 >=1.2.3 <1.2.3 || 1.2.3 <=1.2.3 ~1.2.3 ^1.2.3"
    ])
  ]

runReadP :: ReadP.ReadP a -> String -> Maybe a
runReadP parser string = Maybe.listToMaybe (do
  (x, "") <- ReadP.readP_to_S parser string
  pure x)
