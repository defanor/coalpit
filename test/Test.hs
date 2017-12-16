{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics
import Generic.Random
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Proxy
import Test.Tasty.Travis

import Coalpit


data Basic = Basic Int String Double
  deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary Basic where arbitrary = genericArbitraryU

data WithLists = WithLists [Int] [String] [Double]
  deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary WithLists where arbitrary = genericArbitraryU

data Record = Record { maybeInt :: Maybe Int
                     , maybeDouble :: Maybe Double
                     , str :: String
                     , listOfStrings :: [String]
                     , maybeListOfNumbers :: Maybe [Integer]
                     , otherString :: String
                     } deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary Record where arbitrary = genericArbitraryU

data Sum = Foo Int Bool
         | Bar
         | Baz (String, (Double, Integer), Rational)
  deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary Sum where arbitrary = genericArbitraryU

data Nested = Nested Record Basic WithLists Sum
  deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary Nested where arbitrary = genericArbitraryU

data Polymorphic a b = Polymorphic (Maybe a) [b] (Either a b)
  deriving (Generic, Eq, Show)
instance (Coalpit a, Coalpit b) => Coalpit (Polymorphic a b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Polymorphic a b) where
  arbitrary = genericArbitraryU

data Recursive = RecursiveA
               | RecursiveB Recursive
  deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary Recursive where arbitrary = genericArbitraryU

data NestedRecord = NestedRecord { record1 :: Maybe Record
                                 , record2 :: Maybe Record
                                 , record3 :: Maybe Record
                                 } deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary NestedRecord where arbitrary = genericArbitraryU

data NestedSum = NestedFoo Record
               | NestedBar Sum Basic Nested
               | NestedBaz (Polymorphic Int Double)
               deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary NestedSum where arbitrary = genericArbitraryU

data RecursiveRecordMaybe = RecursiveRecordMaybe
  { rrm :: Maybe RecursiveRecordMaybe
  , record :: Maybe Record
  , guard :: ()
  } deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary RecursiveRecordMaybe where arbitrary = genericArbitraryU

data RecursiveRecordMaybe2 = RecursiveRecordMaybe2
  { record1' :: Maybe Record
  , rrm' :: Maybe RecursiveRecordMaybe2
  , record2' :: Maybe Record
  , guard' :: ()
  } deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary RecursiveRecordMaybe2 where arbitrary = genericArbitraryU

data RecordStrings = RecordStrings
  { s1 :: String
  , s2 :: String
  , s3 :: String
  } deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary RecordStrings where arbitrary = genericArbitraryU

printAndParse :: (Coalpit a, Eq a)
              => Options -> Proxy a -> a -> Bool
printAndParse m _ r = Right r == fromArgs m (toArgs m r)

mkTest :: (Coalpit a, Eq a, Show a, Arbitrary a)
       => Options -> Proxy a -> String -> TestTree
mkTest m p n = QC.testProperty n (printAndParse m p)

idEqToAndFrom :: Options -> TestTree
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

variousOptions :: (Options -> TestTree) -> TestTree
variousOptions tt = testGroup "Various modifiers"
  [ testGroup (concat [ "alwaysUseSelName = ", show ausn
                      , ", omitNamedOptions = ", show ono])
    [tt defOpt { alwaysUseSelName = ausn
               , omitNamedOptions = ono }]
  | ausn <- [True, False]
  , ono <- [True, False]
  ]

qcProps :: TestTree
qcProps = testGroup "Quickcheck properties"
  [ variousOptions idEqToAndFrom ]

main :: IO ()
main = travisTestReporter defaultConfig [] qcProps
