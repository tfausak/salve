import Test.DocTest

main :: IO ()
main = doctest
  [ "library/Salve/Internal.hs"
  , "library/Salve.hs"
  ]
