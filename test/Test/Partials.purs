module Test.Partials where

import Data.Either (Either(..))

forceLeft :: ∀ a b. Partial => Either a b -> a
forceLeft (Left a) = a

forceRight :: ∀ a b. Partial => Either a b -> b
forceRight (Right b) = b
