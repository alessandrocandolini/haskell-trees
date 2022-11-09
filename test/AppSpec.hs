{-# LANGUAGE QuasiQuotes #-}
module AppSpec where

import App (pretty)
import Examples (one, basic1, basic2, basic3, more)
import Test.Hspec ( describe, it, shouldBe, Spec )
import Test.Hspec.QuickCheck ( prop )
import Test.QuickCheck.Property ()
import NeatInterpolation

spec :: Spec
spec = describe "Simple test" $ do

     it "Example 1 (one leaf)" $
        pretty one `shouldBe` [trimming|
        1
        |]

     it "Example 2 (one node, one leaf)" $
        pretty basic1 `shouldBe` [trimming|
           1
           -2
          |]
     
     it "Example 3 (one node, one branch, one leaf)" $
        pretty basic2 `shouldBe` [trimming|
           1
           -2
           --3
          |]

     it "Example 4 (one node, two leafs)" $
        pretty basic3 `shouldBe` [trimming|
           1
           -2
           -3
          |]


     it "Example 5 (a more complex, nested tree)" $
        pretty more `shouldBe` [trimming|
           1
           -2
           --3
           --4
           -5
           --6
           --7
           ---8
           ---9
           ---10
           |]

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])

