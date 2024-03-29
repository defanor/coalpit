{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RankNTypes #-}

import GHC.Generics
import Generic.Random (genericArbitraryU)
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Proxy
import Data.Word
import Data.Int
import Data.Complex

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
                     , maybeNonEmptyListOfNumbers :: Maybe [Integer]
                     , otherString :: String
                     } deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary Record where arbitrary = genericArbitraryU

data Sum = Foo Int Bool
         | Bar
         | Baz (Int8, (Complex Float, Word16), Rational)
  deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary Sum where arbitrary = genericArbitraryU

data Nested = Nested Record Basic WithLists Sum
  deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary Nested where arbitrary = genericArbitraryU

data Polymorphic a b = Polymorphic (Maybe a) (Either a b)
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
               | NestedBaz (Polymorphic Char Double)
               deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary NestedSum where arbitrary = genericArbitraryU

data RecursiveRecordMaybe = RecursiveRecordMaybe
  { rrm :: Maybe RecursiveRecordMaybe
  , record :: Maybe Record
  , guard :: Word8
  } deriving (Generic, Eq, Show, Coalpit)
instance Arbitrary RecursiveRecordMaybe where arbitrary = genericArbitraryU

data RecursiveRecordMaybe2 = RecursiveRecordMaybe2
  { record1' :: Maybe Record
  , rrm' :: Maybe (Maybe RecursiveRecordMaybe2)
  , record2' :: Maybe Record
  , guard' :: Word8
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
printAndParse opt _ r = Right r == fromDSV opt (toDSV opt r)

printAndParseList :: (Coalpit a, Eq a)
                  => Options -> Proxy a -> [a] -> Bool
printAndParseList opt _ l = Right l == fromDSVList opt (toDSVList opt l)

variousTypes :: (forall a. (Coalpit a, Eq a, Show a, Arbitrary a) =>
                  Proxy a -> String -> TestTree)
             -> [TestTree]
variousTypes f =
  [ f (Proxy :: Proxy Basic) "Basic"
  , f (Proxy :: Proxy WithLists) "WithLists"
  , f (Proxy :: Proxy Record) "Record"
  , f (Proxy :: Proxy Sum) "Sum"
  , f (Proxy :: Proxy Nested) "Nested"
  , f (Proxy :: Proxy (Polymorphic Int Double)) "Polymorphic Int Double"
  , f (Proxy :: Proxy (Polymorphic Basic Record)) "Polymorphic Basic Record"
  , f (Proxy :: Proxy (Polymorphic Nested (Polymorphic Basic Sum)))
    "Polymorphic Nested (Polymorphic Basic Sum)"
  , f (Proxy :: Proxy Recursive) "Recursive"
  , f (Proxy :: Proxy NestedRecord) "NestedRecord"
  , f (Proxy :: Proxy NestedSum) "NestedSum"
  , f (Proxy :: Proxy RecursiveRecordMaybe) "RecursiveRecordMaybe"
  , f (Proxy :: Proxy RecursiveRecordMaybe2) "RecursiveRecordMaybe2"
  , f (Proxy :: Proxy RecordStrings) "RecordStrings"
  ]

variousOptions :: (Options -> [TestTree]) -> [TestTree]
variousOptions tt =
  [ testGroup (concat [ "selNamePolicy = ", show snpol ])
    (tt defOpt { selNamePolicy = snpol })
  | snpol <- [SNDisable, SNAvoid, SNPrefer, SNRequire]
  ]

qcProps :: TestTree
qcProps = testGroup "Quickcheck properties"
  [ testGroup "Right == fromDSV opt . toDSV opt"
    (variousOptions $ \opt ->
        variousTypes $ \p n -> QC.testProperty n (printAndParse opt p))
  , testGroup "Right == fromDSVList opt . toDSVList opt"
    (variousOptions $ \opt ->
        variousTypes $ \p n -> QC.testProperty n (printAndParseList opt p))
  ]

main :: IO ()
main = defaultMain qcProps
