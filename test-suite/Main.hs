-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty as T
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import qualified Test.Tasty.Hspec as HS
import qualified Test.Tasty.QuickCheck as QC
-- import qualified Test.Tasty.SmallCheck as SC

import Data.List (sort)
import Husk

main :: IO ()
main = do
  orderedSpecs <- orderedSpecsM
  insertOrderedSpecs <- insertOrderedSpecsM
  T.defaultMain $ T.testGroup "HuskTests" ([orderedProps, orderedSpecs, insertOrderedProps, insertOrderedSpecs])
  where
    orderedProps :: T.TestTree
    orderedProps = T.testGroup "OrderedProps"
      [ QC.testProperty "Int"    $ \xs -> (QC.classify (null xs) "empty" $ (ordered . sort $ (xs :: [Int])))
      , QC.testProperty "Double" $ \xs -> (QC.classify (null xs) "empty" $ (ordered . sort $ (xs :: [Double])))
      , QC.testProperty "String" $ \xs -> (QC.classify (null xs) "empty" $ (ordered . sort $ (xs :: [String])))
      ]

    orderedSpecsM :: IO T.TestTree
    orderedSpecsM = T.testGroup "OrderedSpecs" <$> sequence [ HS.testSpec "empty" $ HS.parallel $ do
                                                                HS.it "Int" $ HS.shouldBe (ordered( [] :: [Int])) True
                                                                HS.it "String" $ HS.shouldBe (ordered( [] :: [String])) True
                                                            , HS.testSpec "List" $ HS.parallel $ do
                                                                HS.it "Int" $ HS.shouldBe (ordered([0, 1, 2, 3] :: [Int])) True
                                                                HS.it "String" $ HS.shouldBe (ordered(["adrien", "pedro"])) True
                                                            , HS.testSpec "Singleton" $ HS.parallel $ do
                                                                HS.it "Int" $ HS.shouldBe (ordered([0] :: [Int])) True
                                                                HS.it "String" $ HS.shouldBe (ordered(["adrien"])) True
                                                            ]
    insertOrderedProps :: T.TestTree
    insertOrderedProps = T.testGroup "InsertOrderedProps"
      [ QC.testProperty "Int" (((.).(.)) ordered insertOrdered :: Int -> [Int] -> Bool)
      , QC.testProperty "Double" (((.).(.)) ordered insertOrdered :: Double -> [Double] -> Bool)
      , QC.testProperty "String" (((.).(.)) ordered insertOrdered :: String -> [String] -> Bool)
      ]

    insertOrderedSpecsM :: IO T.TestTree
    insertOrderedSpecsM = T.testGroup "InsertOrderedSpecs" <$> sequence [ HS.testSpec "empty" $ HS.parallel $ do
                                                                            HS.it "Int" $ HS.shouldBe (insertOrdered (1 :: Int) []) [1]
                                                                        , HS.testSpec "singleton" $ do
                                                                            HS.it "Ordered List" $ HS.shouldBe (insertOrdered (1::Int) [2]) [1,2]
                                                                            HS.it "Unordered List" $ HS.shouldBe (insertOrdered (2::Int) [1]) [1, 2]
                                                                        ]
