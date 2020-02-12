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
import           Data.Dependent.Map as D
import           Data.NonEmpty.Dependent.Map as DM
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

newtype EmailAddress = EmailAddress String
  deriving (Eq, Ord, Show, Arbitrary)
newtype FacebookId = FacebookId String
  deriving (Eq, Ord, Show, Arbitrary)
newtype AppleToken = AppleToken String
  deriving (Eq, Ord, Show, Arbitrary)


-- | This is the tag used as key in the NonEmptyDMap
data AuthMethod a where
  EmailAuth    :: AuthMethod EmailAddress
  FacebookAuth :: AuthMethod FacebookId
  AppleAuth    :: AuthMethod AppleToken

-- | This is a non-empty dependent map keyed by AuthMethod and valued by the
-- type associated with each AuthMethod (eg: EmailAuth->EmailAddress,
-- FacebookAuth->FacebookId, ...)
-- 
-- Example:
--  let m = (EmailAuth :==> EmailAddress "foo@bar")
--      :|> fromList [ FacebookAuth :==> FacebookId "foobar" ]
type AuthMethodMap = NonEmptyDMap AuthMethod Identity

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

  prop "head is always the smallest element" $ \((x:|>xs) :: AuthMethodMap) ->
    not (D.null xs) ==> x `shouldSatisfy` (< D.findMin xs)

  prop "fromList . toList = id" $ \(m :: AuthMethodMap) ->
    (DM.fromList . DM.toList) m `shouldBe` m



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
