import Test.DocTest

main = doctest args

args = ghcOptions ++ sourceFiles

ghcOptions =
  [ "-XDeriveGeneric"
  , "-XDeriveLift"
  , "-XDerivingStrategies"
  , "-XGeneralizedNewtypeDeriving"
  , "-XNoImplicitPrelude"
  , "-XQuasiQuotes"
  , "-XStandaloneDeriving"
  ]

sourceFiles =
  [ "ascii/ASCII.hs"
  , "ascii-case/ASCII/Case.hs"
  , "ascii-char/ASCII/Char.hs"
  , "ascii-group/ASCII/Group.hs"
  , "ascii-predicates/ASCII/Lists.hs"
  , "ascii-predicates/ASCII/Predicates.hs"
  , "ascii-predicates/ASCII/ListsAndPredicates.hs"
  , "ascii-superset/ASCII/Refinement.hs"
  , "ascii-superset/ASCII/Superset.hs"
  , "ascii-th/ASCII/QuasiQuoters.hs"
  , "ascii-th/ASCII/TemplateHaskell.hs"
  ]
