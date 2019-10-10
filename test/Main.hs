module Main(main) where

import qualified Spec

-- WORKAROUND https://github.com/haskell/cabal/issues/5103
main :: IO ()
main = Spec.main
