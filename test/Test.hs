{-# LANGUAGE DeriveGeneric #-}

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
                     , maybeListOfNumbers :: Maybe [Integer]
                     , otherString :: String
                     } deriving (Generic, Eq, Show)
instance ArgParser Record
instance ToArgs Record
instance Arbitrary Record where arbitrary = genericArbitraryU

data Sum = Foo Int Bool
         | Bar
         | Baz (String, (Double, Integer), Rational)
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

data RecursiveRecordMaybe = RecursiveRecordMaybe
  { rrm :: Maybe RecursiveRecordMaybe
  , record :: Maybe Record
  , guard :: ()
  } deriving (Generic, Eq, Show)
instance ArgParser RecursiveRecordMaybe
instance ToArgs RecursiveRecordMaybe
instance Arbitrary RecursiveRecordMaybe where arbitrary = genericArbitraryU

data RecursiveRecordMaybe2 = RecursiveRecordMaybe2
  { record1' :: Maybe Record
  , rrm' :: Maybe RecursiveRecordMaybe2
  , record2' :: Maybe Record
  , guard' :: ()
  } deriving (Generic, Eq, Show)
instance ArgParser RecursiveRecordMaybe2
instance ToArgs RecursiveRecordMaybe2
instance Arbitrary RecursiveRecordMaybe2 where arbitrary = genericArbitraryU

data RecordStrings = RecordStrings
  { s1 :: String
  , s2 :: String
  , s3 :: String
  } deriving (Generic, Eq, Show)
instance ArgParser RecordStrings
instance ToArgs RecordStrings
instance Arbitrary RecordStrings where arbitrary = genericArbitraryU

printAndParse :: (ArgParser a, ToArgs a, Eq a)
              => Modifiers -> Proxy a -> a -> Bool
printAndParse m _ r = Right r == fromArgs m (toArgs m r)

mkTest :: (ArgParser a, ToArgs a, Eq a, Show a, Arbitrary a)
       => Modifiers -> Proxy a -> String -> TestTree
mkTest m p n = QC.testProperty n (printAndParse m p)

idEqToAndFrom :: Modifiers -> TestTree
idEqToAndFrom m = testGroup "id == parse . print"
  [ mkTest m (Proxy :: Proxy Basic) "Basic"
  , mkTest m (Proxy :: Proxy WithLists) "WithLists"
  , mkTest m (Proxy :: Proxy Record) "Record"
  , mkTest m (Proxy :: Proxy Sum) "Sum"
  , mkTest m (Proxy :: Proxy Nested) "Nested"
  , mkTest m (Proxy :: Proxy (Polymorphic Int Double))
    "Polymorphic Int Double"
  , mkTest m (Proxy :: Proxy (Polymorphic Basic Record))
    "Polymorphic Basic Record"
  , mkTest m (Proxy :: Proxy (Polymorphic Nested (Polymorphic Basic Sum)))
    "Polymorphic Nested (Polymorphic Basic Sum)"
  , mkTest m (Proxy :: Proxy Recursive) "Recursive"
  , mkTest m (Proxy :: Proxy NestedRecord) "NestedRecord"
  , mkTest m (Proxy :: Proxy NestedSum) "NestedSum"
  , mkTest m (Proxy :: Proxy RecursiveRecordMaybe) "RecursiveRecordMaybe"
  , mkTest m (Proxy :: Proxy RecursiveRecordMaybe2) "RecursiveRecordMaybe2"
  , mkTest m (Proxy :: Proxy RecordStrings) "RecordStrings"
  ]

variousModifiers :: (Modifiers -> TestTree) -> TestTree
variousModifiers tt = testGroup "Various modifiers"
  [ testGroup "alwaysUseSelName = True"
    [tt defMod { alwaysUseSelName = True }]
  , testGroup "alwaysUseSelName = False"
    [tt defMod { alwaysUseSelName = False }]
  ]

qcProps :: TestTree
qcProps = testGroup "Quickcheck properties"
  [ variousModifiers idEqToAndFrom ]

main :: IO ()
main = travisTestReporter defaultConfig [] qcProps
