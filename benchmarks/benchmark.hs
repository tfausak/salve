{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.DeepSeq
import Criterion.Main
import GHC.Generics
import Salve.Internal

deriving instance Generic Build
deriving instance Generic Constraint
deriving instance Generic Operator
deriving instance Generic PreRelease
deriving instance Generic Version

deriving instance NFData Build
deriving instance NFData Constraint
deriving instance NFData Operator
deriving instance NFData PreRelease
deriving instance NFData Version

main :: IO ()
main = defaultMain
  [ bgroup "parseVersion" (map
    (\ x -> bench x (nf parseVersion x))
    [ "0.0.0"
    , "123.456.789-pre.release+build.metadata"
    ])
  , bgroup "parseConstraint" (map
    (\ x -> bench x (nf parseConstraint x))
    [ ">=0.0.0"
    , ">1.2.3 || =1.2.3 >=1.2.3 <1.2.3 || 1.2.3 <=1.2.3 ~1.2.3 ^1.2.3"
    ])
  , bgroup "renderVersion" (map
    (\ x -> let y = unsafeParseVersion x in bench x (nf renderVersion y))
    [ "0.0.0"
    , "123.456.789-pre.release+build.metadata"
    ])
  , bgroup "renderConstraint" (map
    (\ x -> let y = unsafeParseConstraint x in bench x (nf renderConstraint y))
    [ ">=0.0.0"
    , ">1.2.3 || =1.2.3 >=1.2.3 <1.2.3 || 1.2.3 <=1.2.3 ~1.2.3 ^1.2.3"
    ])
  ]
