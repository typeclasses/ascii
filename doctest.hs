import Test.DocTest

main = doctest args

args = ghcOptions ++ sourceFiles

ghcOptions =
  [ "-XDeriveGeneric"
  , "-XDeriveLift"
  , "-XNoImplicitPrelude"
  , "-XQuasiQuotes"
  , "-XStandaloneDeriving"
  ]

sourceFiles =
  [ "ascii/ASCII.hs"
  , "ascii/ASCII/Lists.hs"
  , "ascii/ASCII/Predicates.hs"
  , "ascii-case/ASCII/Case.hs"
  , "ascii-char/ASCII/Char.hs"
  , "ascii-group/ASCII/Group.hs"
  , "ascii-superset/ASCII/Refinement.hs"
  , "ascii-superset/ASCII/Superset.hs"
  , "ascii-th/ASCII/QuasiQuoters.hs"
  , "ascii-th/ASCII/TemplateHaskell.hs"
  ]
