module SyncSpec (spec) where

import Data.Text (Text)
import GoogleGroups (GroupMember (..), GroupMemberRole (..))
import Sync (SyncPlan (..), computeSyncPlan)
import Test.Hspec

--------------------------------------------------------------------------------

-- | Build a GroupMember with the given email and role. Type/status are
-- irrelevant to reconciliation, so use fixed placeholders.
member :: Text -> GroupMemberRole -> GroupMember
member email role =
  GroupMember
    { gmEmail = email,
      gmRole = role,
      gmType = "USER",
      gmStatus = "ACTIVE"
    }

spec :: Spec
spec = describe "computeSyncPlan" $ do
  it "protects a MANAGER who is not a host (not removed)" $ do
    let plan = computeSyncPlan [member "boss@kpbj.fm" RoleManager] []
    spToRemove plan `shouldBe` []
    spProtected plan `shouldBe` ["boss@kpbj.fm"]

  it "protects an OWNER who is not a host" $ do
    let plan = computeSyncPlan [member "owner@kpbj.fm" RoleOwner] []
    spToRemove plan `shouldBe` []
    spProtected plan `shouldBe` ["owner@kpbj.fm"]

  it "removes a plain MEMBER who is not a host" $ do
    let plan = computeSyncPlan [member "ex@example.com" RoleMember] []
    spToRemove plan `shouldBe` ["ex@example.com"]
    spProtected plan `shouldBe` []

  it "adds a host missing from the group" $ do
    let plan = computeSyncPlan [] ["newhost@example.com"]
    spToAdd plan `shouldBe` ["newhost@example.com"]

  it "matches emails case-insensitively (no spurious add or remove)" $ do
    let plan = computeSyncPlan [member "Host@KPBJ.FM" RoleMember] ["host@kpbj.fm"]
    spToAdd plan `shouldBe` []
    spToRemove plan `shouldBe` []

  it "does not remove or double-report a MANAGER who is also a host" $ do
    let plan = computeSyncPlan [member "dj@kpbj.fm" RoleManager] ["dj@kpbj.fm"]
    spToRemove plan `shouldBe` []
    spProtected plan `shouldBe` []

  it "composes add/remove/protect cleanly with a mixed membership" $ do
    let groupMembers =
          [ member "boss@kpbj.fm" RoleManager, -- protected (not a host)
            member "owner@kpbj.fm" RoleOwner, -- protected (not a host)
            member "gone@example.com" RoleMember, -- removed (not a host)
            member "dj@example.com" RoleMember -- kept (is a host)
          ]
        hosts =
          [ "dj@example.com", -- already a member, no-op
            "newhost@example.com" -- missing from the group, added
          ]
        plan = computeSyncPlan groupMembers hosts
    spToAdd plan `shouldBe` ["newhost@example.com"]
    spToRemove plan `shouldBe` ["gone@example.com"]
    spProtected plan `shouldBe` ["boss@kpbj.fm", "owner@kpbj.fm"]
