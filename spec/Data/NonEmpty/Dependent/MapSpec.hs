{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.NonEmpty.Dependent.MapSpec  where


import           Data.Constraint.Extras      (Has')
import           Data.Constraint.Extras.TH
import           Data.Functor.Identity       (Identity)
import           Data.GADT.Compare.TH
import           Data.GADT.Show
import           Data.GADT.Show.TH
import           Data.List.NonEmpty
import           Data.NonEmpty.Dependent.Map as DM
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

newtype EmailAddress = EmailAddress String
  deriving (Eq, Show, Arbitrary)
newtype FacebookId = FacebookId String
  deriving (Eq, Show, Arbitrary)
newtype AppleToken = AppleToken String
  deriving (Eq, Show, Arbitrary)


-- | This is the tag used as key in the NonEmptyDMap
data AuthMethod a where
  EmailAuth :: AuthMethod EmailAddress
  FacebookAuth :: AuthMethod FacebookId
  AppleAuth :: AuthMethod AppleToken

$(deriveGEq ''AuthMethod)
$(deriveGCompare ''AuthMethod)
$(deriveGShow ''AuthMethod)
$(deriveArgDict ''AuthMethod)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "NonEmptyDMap" $ do
  prop "always has size >= 1" $ \(m :: AuthMethodMap) ->
    DM.size m `shouldSatisfy` (>=1)

type AuthMethodMap = NonEmptyDMap AuthMethod Identity

instance Has' Arbitrary AuthMethod f => Arbitrary (DSum AuthMethod f) where
  arbitrary = oneof
    [ (EmailAuth :=>) <$> arbitrary
    , (FacebookAuth :=>) <$> arbitrary
    , (AppleAuth :=>) <$> arbitrary
    ]

instance Has' Arbitrary AuthMethod f => Arbitrary (NonEmptyDMap AuthMethod f) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    pure (DM.fromList (x:|xs))
