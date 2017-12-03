{-# LANGUAGE DeriveGeneric #-}

import Text.Megaparsec
import GHC.Generics
import Generic.Random
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Proxy
import Test.Tasty.Travis

import Coalpit


data Basic = Basic Int String Double
  deriving (Generic, Eq, Show)
instance ArgParser Basic
instance ToArgs Basic
instance Arbitrary Basic where arbitrary = genericArbitraryU

data WithLists = WithLists [Int] [String] [Double]
  deriving (Generic, Eq, Show)
instance ArgParser WithLists
instance ToArgs WithLists
instance Arbitrary WithLists where arbitrary = genericArbitraryU

data Record = Record { maybeInt :: Maybe Int
                     , maybeDouble :: Maybe Double
                     , str :: String
                     , listOfStrings :: [String]
                     , maybeListOfNumbers :: Maybe [Int]
                     , otherString :: String
                     } deriving (Generic, Eq, Show)
instance ArgParser Record
instance ToArgs Record
instance Arbitrary Record where arbitrary = genericArbitraryU

data Sum = Foo Int
          | Bar
          | Baz String Double
  deriving (Generic, Eq, Show)
instance ArgParser Sum
instance ToArgs Sum
instance Arbitrary Sum where arbitrary = genericArbitraryU

data Nested = Nested Record Basic WithLists Sum
  deriving (Generic, Eq, Show)
instance ArgParser Nested
instance ToArgs Nested
instance Arbitrary Nested where arbitrary = genericArbitraryU

data Polymorphic a b = Polymorphic (Maybe a) [b] (Either a b)
  deriving (Generic, Eq, Show)
instance (ArgParser a, ArgParser b) => ArgParser (Polymorphic a b)
instance (ToArgs a, ToArgs b) => ToArgs (Polymorphic a b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Polymorphic a b) where
  arbitrary = genericArbitraryU

data Recursive = RecursiveA
               | RecursiveB Recursive
  deriving (Generic, Eq, Show)
instance ArgParser Recursive
instance ToArgs Recursive
instance Arbitrary Recursive where arbitrary = genericArbitraryU

data NestedRecord = NestedRecord { record1 :: Maybe Record
                                 , record2 :: Maybe Record
                                 , record3 :: Maybe Record
                                 } deriving (Generic, Eq, Show)
instance ArgParser NestedRecord
instance ToArgs NestedRecord
instance Arbitrary NestedRecord where arbitrary = genericArbitraryU

data NestedSum = NestedFoo Record
                | NestedBar Sum Basic Nested
                | NestedBaz (Polymorphic Int Double)
                deriving (Generic, Eq, Show)
instance ArgParser NestedSum
instance ToArgs NestedSum
instance Arbitrary NestedSum where arbitrary = genericArbitraryU

printAndParse :: (ArgParser a, ToArgs a, Eq a, Show a, Arbitrary a)
              => Proxy a -> a -> Bool
printAndParse _ r = Right r == parse (argParser defMod) "test" (toArgs defMod r)

mkTest :: (ArgParser a, ToArgs a, Eq a, Show a, Arbitrary a)
       => Proxy a -> String -> TestTree
mkTest p n = QC.testProperty ("id == parse . print for " ++ n) (printAndParse p)

main :: IO ()
main = travisTestReporter defaultConfig [] qcProps

qcProps = testGroup "Quickcheck properties"
  [ mkTest (Proxy :: Proxy Basic) "Basic"
  , mkTest (Proxy :: Proxy WithLists) "WithLists"
  , mkTest (Proxy :: Proxy Record) "Record"
  , mkTest (Proxy :: Proxy Sum) "Sum"
  , mkTest (Proxy :: Proxy Nested) "Nested"
  , mkTest (Proxy :: Proxy (Polymorphic Int Double))
    "Polymorphic Int Double"
  , mkTest (Proxy :: Proxy (Polymorphic Basic Record))
    "Polymorphic Basic Record"
  , mkTest (Proxy :: Proxy (Polymorphic Nested (Polymorphic Basic Sum)))
    "Polymorphic Nested (Polymorphic Basic Sum)"
  , mkTest (Proxy :: Proxy Recursive) "Recursive"
  , mkTest (Proxy :: Proxy NestedRecord) "NestedRecord"
  , mkTest (Proxy :: Proxy NestedSum) "NestedSum"
  ]
