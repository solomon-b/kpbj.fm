module Effects.Database.Tables.UserRoleSpec (spec) where

--------------------------------------------------------------------------------

import Effects.Database.Tables.UserMetadata (UserRole (..), isAdmin, isHostOrHigher, isStaffOrHigher)
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Effects.Database.Tables.UserMetadata.UserRole" $ do
  describe "Role hierarchy functions" $ do
    describe "isAdmin" $ do
      it "returns True only for Admin role" $ do
        isAdmin Admin `shouldBe` True
        isAdmin Staff `shouldBe` False
        isAdmin Host `shouldBe` False
        isAdmin User `shouldBe` False

    describe "isStaffOrHigher" $ do
      it "returns True for Staff and Admin" $ do
        isStaffOrHigher Admin `shouldBe` True
        isStaffOrHigher Staff `shouldBe` True
        isStaffOrHigher Host `shouldBe` False
        isStaffOrHigher User `shouldBe` False

    describe "isHostOrHigher" $ do
      it "returns True for Host, Staff, and Admin" $ do
        isHostOrHigher Admin `shouldBe` True
        isHostOrHigher Staff `shouldBe` True
        isHostOrHigher Host `shouldBe` True
        isHostOrHigher User `shouldBe` False

  describe "Role ordering" $ do
    it "maintains correct hierarchy order" $ do
      -- Test that roles are ordered correctly (User < Host < Staff < Admin)
      User < Host `shouldBe` True
      Host < Staff `shouldBe` True
      Staff < Admin `shouldBe` True
      User < Admin `shouldBe` True

      -- Test reverse is false
      Admin < User `shouldBe` False
      Staff < Host `shouldBe` False

  describe "Role enumeration" $ do
    it "includes all expected roles" $ do
      [minBound .. maxBound] `shouldBe` [User, Host, Staff, Admin]

    it "has correct enum values" $ do
      fromEnum User `shouldBe` 0
      fromEnum Host `shouldBe` 1
      fromEnum Staff `shouldBe` 2
      fromEnum Admin `shouldBe` 3
