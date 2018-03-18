import qualified Test.DocTest as Doctest

main :: IO ()
main = Doctest.doctest
  [ "library/Salve/Internal.hs"
  , "library/Salve.hs"
  ]
