import qualified Control.Monad as Monad
import qualified Salve
import qualified System.Exit as Exit
import qualified Test.HUnit as Test

main :: IO ()
main = do
  counts <- Test.runTestTT $ Test.TestList
    [ (compare <$> Salve.parseVersion "1.2.3" <*> Salve.parseVersion "2.0.0")
      Test.~?= Just LT
    , (compare <$> Salve.parseVersion "1.2.3" <*> Salve.parseVersion "1.3.0")
      Test.~?= Just LT
    , (compare <$> Salve.parseVersion "1.2.3" <*> Salve.parseVersion "1.2.4")
      Test.~?= Just LT
    , (compare <$> Salve.parseVersion "0.0.9" <*> Salve.parseVersion "0.0.10")
      Test.~?= Just LT
    , (compare <$> Salve.parseVersion "1.2.3-a" <*> Salve.parseVersion
        "1.2.3-b"
      )
      Test.~?= Just LT
    , (compare <$> Salve.parseVersion "1.2.3-pre" <*> Salve.parseVersion
        "1.2.3"
      )
      Test.~?= Just LT
    , (compare <$> Salve.parseVersion "1.2.4-pre" <*> Salve.parseVersion
        "1.2.3"
      )
      Test.~?= Just GT
    , (compare <$> Salve.parseVersion "1.2.3+a" <*> Salve.parseVersion
        "1.2.3+b"
      )
      Test.~?= Just EQ
    , ((==) <$> Salve.parseVersion "1.2.3+a" <*> Salve.parseVersion "1.2.3+b")
      Test.~?= Just False
    , (compare <$> Salve.parsePreRelease "1" <*> Salve.parsePreRelease "a")
      Test.~?= Just LT
    , (compare <$> Salve.parsePreRelease "9" <*> Salve.parsePreRelease "10")
      Test.~?= Just LT
    , (compare <$> Salve.parsePreRelease "p10" <*> Salve.parsePreRelease "p9")
      Test.~?= Just LT
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "<1.2.3"
      <*> Salve.parseVersion "1.2.2"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "<1.2.3"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "<1.2.3"
      <*> Salve.parseVersion "1.2.4"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "<1.2.3"
      <*> Salve.parseVersion "1.2.3-pre"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "<=1.2.3"
      <*> Salve.parseVersion "1.2.2"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "<=1.2.3"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "<=1.2.3"
      <*> Salve.parseVersion "1.2.4"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "=1.2.3"
      <*> Salve.parseVersion "1.2.2"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "=1.2.3"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "=1.2.3"
      <*> Salve.parseVersion "1.2.4"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "=1.2.3"
      <*> Salve.parseVersion "1.2.3-pre"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "=1.2.3"
      <*> Salve.parseVersion "1.2.3+build"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint ">=1.2.3"
      <*> Salve.parseVersion "1.2.2"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint ">=1.2.3"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint ">=1.2.3"
      <*> Salve.parseVersion "1.2.4"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint ">1.2.3"
      <*> Salve.parseVersion "1.2.2"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint ">1.2.3"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint ">1.2.3"
      <*> Salve.parseVersion "1.2.4"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint ">1.2.3"
      <*> Salve.parseVersion "1.2.4-pre"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint ">1.2.3-pre"
      <*> Salve.parseVersion "1.2.4"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint ">1.2.3 <1.2.5"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint ">1.2.3 <1.2.5"
      <*> Salve.parseVersion "1.2.4"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint ">1.2.3 <1.2.5"
      <*> Salve.parseVersion "1.2.5"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.3 || 1.2.4"
      <*> Salve.parseVersion "1.2.2"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.3 || 1.2.4"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.3 || 1.2.4"
      <*> Salve.parseVersion "1.2.4"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.3 || 1.2.4"
      <*> Salve.parseVersion "1.2.5"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.2 || >1.2.3 <1.3.0"
      <*> Salve.parseVersion "1.2.2"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.2 || >1.2.3 <1.3.0"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.2 || >1.2.3 <1.3.0"
      <*> Salve.parseVersion "1.2.4"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.2 || >1.2.3 <1.3.0"
      <*> Salve.parseVersion "1.3.0"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.3 - 1.2.4"
      <*> Salve.parseVersion "1.2.2"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.3 - 1.2.4"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.3 - 1.2.4"
      <*> Salve.parseVersion "1.2.4"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.3 - 1.2.4"
      <*> Salve.parseVersion "1.2.5"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "~1.2.3"
      <*> Salve.parseVersion "1.2.2"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "~1.2.3"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "~1.2.3"
      <*> Salve.parseVersion "1.2.4"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "~1.2.3"
      <*> Salve.parseVersion "1.3.0"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "^1.2.3"
      <*> Salve.parseVersion "1.2.2"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "^1.2.3"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "^1.2.3"
      <*> Salve.parseVersion "1.2.4"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "^1.2.3"
      <*> Salve.parseVersion "1.3.0"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "^1.2.3"
      <*> Salve.parseVersion "2.0.0"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "^0.2.3"
      <*> Salve.parseVersion "0.2.2"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "^0.2.3"
      <*> Salve.parseVersion "0.2.3"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "^0.2.3"
      <*> Salve.parseVersion "0.2.4"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "^0.2.3"
      <*> Salve.parseVersion "0.3.0"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "^0.0.3"
      <*> Salve.parseVersion "0.0.2"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "^0.0.3"
      <*> Salve.parseVersion "0.0.3"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "^0.0.3"
      <*> Salve.parseVersion "0.0.4"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.x"
      <*> Salve.parseVersion "1.1.0"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.x"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.2.x"
      <*> Salve.parseVersion "1.3.0"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.x.x"
      <*> Salve.parseVersion "0.1.0"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.x.x"
      <*> Salve.parseVersion "1.0.0"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.x.x"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "1.x.x"
      <*> Salve.parseVersion "2.0.0"
      )
      Test.~?= Just False
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "x.x.x"
      <*> Salve.parseVersion "0.0.0"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "x.x.x"
      <*> Salve.parseVersion "1.2.3"
      )
      Test.~?= Just True
    , (Salve.satisfiesConstraint
      <$> Salve.parseConstraint "x.x.x"
      <*> Salve.parseVersion "2.0.0"
      )
      Test.~?= Just True
    ]

  let
    hasErrors = Test.errors counts /= 0
    hasFailures = Test.failures counts /= 0
  Monad.when (hasErrors || hasFailures) Exit.exitFailure
