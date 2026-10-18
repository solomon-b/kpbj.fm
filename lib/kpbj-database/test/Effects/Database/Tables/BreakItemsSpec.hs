{-# LANGUAGE QuasiQuotes #-}

-- | Tests for the @break_items@ constraints and the show promo queries.
--
-- A show promo is the one category with a show and a review. These tests
-- cover:
--
-- 1. The constraint that ties each category to its own fields
-- 2. The constraints on the review status and the review note
-- 3. The filter in 'UUT.getActiveInMonth' that keeps a promo off the air
--    unless staff approved it and its show is live
-- 4. Approve and reject, which change only a pending promo
-- 5. The two promo lists
module Effects.Database.Tables.BreakItemsSpec where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import Domain.Types.Slug (mkSlug)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BreakItems qualified as UUT
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Effects.Database.Tables.User qualified as User
import Hasql.Interpolate (OneRow (..), interp, sql)
import Hasql.Pool (UsageError)
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.BreakItems" $ do
      describe "the category fields constraint" $ do
        it "accepts each category with its own fields" acceptsCategoryFields
        it "refuses a show promo with no show" refusesPromoWithoutShow
        it "refuses a PSA with a show" refusesPsaWithShow
        it "refuses an announcement with an underwriter" refusesAnnouncementWithUnderwriter
        it "refuses an underwriting announcement with a show" refusesUnderwritingWithShow

      describe "the review constraints" $ do
        it "refuses a PSA that is not approved" refusesPendingPsa
        it "refuses a review note on an approved promo" refusesNoteOnApprovedPromo

      describe "getActiveInMonth" $ do
        it "includes an approved promo of an active show" includesApprovedPromo
        it "leaves out a pending promo" excludesPendingPromo
        it "leaves out a rejected promo" excludesRejectedPromo
        it "leaves out a promo of an inactive show" excludesInactiveShowPromo
        it "leaves out a promo of a deleted show" excludesDeletedShowPromo
        it "includes a PSA and an announcement, which have no show" includesItemsWithoutShow

      describe "approvePromo and rejectPromo" $ do
        it "approves a pending promo" approvesPending
        it "rejects a pending promo with a note" rejectsPending
        it "does not approve a promo that is already approved" approveTwiceDoesNothing
        it "does not reject an approved promo" rejectApprovedDoesNothing
        it "does not approve a rejected promo" approveRejectedDoesNothing
        it "does not approve a PSA" approvePsaDoesNothing
        it "does not approve a deleted promo" approveDeletedDoesNothing

      describe "promo lists" $ do
        it "getPromosForShow lists one show's live promos, newest first" promosForShow
        it "getPendingPromos lists live pending promos, oldest first" pendingPromos
        it "getReviewedPromos lists reviewed promos newest first" reviewedPromos

--------------------------------------------------------------------------------
-- Fixtures

-- | Monday, January 6, 2025. Every fixture item is active on this day.
testDay :: Day
testDay = fromGregorian 2025 1 6

-- | The rows that every case needs.
data Fixture = Fixture
  { fxCreator :: User.Id,
    fxShow :: Shows.Id,
    fxUnderwriter :: Underwriters.Id
  }

-- | Insert a staff user, an active show, and an underwriter.
setup :: TRX.Transaction Fixture
setup = do
  creatorId <- insertCreator
  showId <- insertShow "night-shift" Shows.Active
  underwriterId <- unwrapInsert (Underwriters.insertUnderwriter "Sun Valley Hardware")
  pure
    Fixture
      { fxCreator = creatorId,
        fxShow = showId,
        fxUnderwriter = underwriterId
      }

-- | Insert the user who uploads every item.
--
-- The break item tests read no user fields, so a plain row is enough.
insertCreator :: TRX.Transaction User.Id
insertCreator = do
  OneRow userId <-
    TRX.statement () $
      interp
        False
        [sql|
          INSERT INTO users (email, password)
          VALUES ('programming@kpbj.fm', 'not-a-real-hash')
          RETURNING id
        |]
  pure userId

-- | Insert a show with the given slug and status.
insertShow :: Text -> Shows.Status -> TRX.Transaction Shows.Id
insertShow slug status =
  unwrapInsert $
    Shows.insertShow
      Shows.Insert
        { siTitle = "Show " <> slug,
          siSlug = mkSlug slug,
          siDescription = Nothing,
          siLogoUrl = Nothing,
          siStatus = status
        }

-- | Soft delete a show.
deleteShow :: Shows.Id -> TRX.Transaction ()
deleteShow showId =
  TRX.statement () $
    interp False [sql| UPDATE shows SET deleted_at = NOW() WHERE id = #{showId} |]

-- | Write a review note on a break item and change nothing else.
setReviewNote :: UUT.Id -> Text -> TRX.Transaction ()
setReviewNote itemId note =
  TRX.statement () $
    interp False [sql| UPDATE break_items SET review_note = #{note} WHERE id = #{itemId} |]

-- | A 30 second PSA that runs from January 1, 2025 with no end.
psaInsert :: User.Id -> UUT.Insert
psaInsert creatorId =
  UUT.Insert
    { biiTitle = "Library Hours",
      biiCategory = UUT.Psa,
      biiUnderwriterId = Nothing,
      biiShowId = Nothing,
      biiReviewStatus = UUT.Approved,
      biiAudioFilePath = "audio/break-items/2025/01/01/library-hours_2025-01-01_abc.mp3",
      biiMimeType = "audio/mpeg",
      biiFileSize = 480000,
      biiDurationSeconds = 30,
      biiStartsOn = fromGregorian 2025 1 1,
      biiEndsOn = Nothing,
      biiCreatorId = creatorId
    }

-- | A station announcement for a fund drive.
announcementInsert :: User.Id -> UUT.Insert
announcementInsert creatorId =
  (psaInsert creatorId)
    { UUT.biiTitle = "Fall Fund Drive",
      UUT.biiCategory = UUT.Announcement,
      UUT.biiAudioFilePath = "audio/break-items/2025/01/01/fund-drive_2025-01-01_def.mp3"
    }

-- | An underwriting announcement for the given underwriter.
underwritingInsert :: User.Id -> Underwriters.Id -> UUT.Insert
underwritingInsert creatorId underwriterId =
  (psaInsert creatorId)
    { UUT.biiTitle = "Sun Valley Hardware",
      UUT.biiCategory = UUT.Underwriting,
      UUT.biiUnderwriterId = Just underwriterId,
      UUT.biiAudioFilePath = "audio/break-items/2025/01/01/hardware_2025-01-01_ghi.mp3"
    }

-- | An 8 second show promo for the given show, with the given review status.
promoInsert :: User.Id -> Shows.Id -> UUT.ReviewStatus -> UUT.Insert
promoInsert creatorId showId status =
  (psaInsert creatorId)
    { UUT.biiTitle = "Tune In To Night Shift",
      UUT.biiCategory = UUT.ShowPromo,
      UUT.biiDurationSeconds = 8,
      UUT.biiShowId = Just showId,
      UUT.biiReviewStatus = status,
      UUT.biiAudioFilePath = "audio/break-items/2025/01/01/night-shift_2025-01-01_jkl.mp3"
    }

-- | Insert a break item and return its ID.
insertItem :: UUT.Insert -> TRX.Transaction UUT.Id
insertItem = unwrapInsert . UUT.insertBreakItem

-- | Run one write transaction on a fresh connection.
runTransaction :: TestDBConfig -> TRX.Transaction a -> IO (Either UsageError a)
runTransaction cfg transaction =
  bracketConn cfg $ runDB $ TRX.transaction TRX.ReadCommitted TRX.Write transaction

-- | Run a transaction and fail the test on a database error.
runOrFail :: TestDBConfig -> TRX.Transaction a -> IO a
runOrFail cfg transaction = do
  result <- runTransaction cfg transaction
  case result of
    Left err -> error ("DB error: " <> show err)
    Right value -> pure value

-- | Run a transaction and expect the database to refuse it.
expectRefused :: TestDBConfig -> TRX.Transaction a -> IO ()
expectRefused cfg transaction = do
  result <- runTransaction cfg transaction
  case result of
    Left _ -> pure ()
    Right _ -> expectationFailure "The database accepted a row that it must refuse."

-- | The IDs of the items that 'UUT.getActiveInMonth' returns for 'testDay'.
activeIds :: TRX.Transaction [UUT.Id]
activeIds = do
  items <- TRX.statement () (UUT.getActiveInMonth testDay testDay)
  pure (map UUT.bimId items)

--------------------------------------------------------------------------------
-- The Category Fields Constraint

acceptsCategoryFields :: TestDBConfig -> IO ()
acceptsCategoryFields cfg = do
  categories <- runOrFail cfg $ do
    fixture <- setup
    psaId <- insertItem (psaInsert (fxCreator fixture))
    announcementId <- insertItem (announcementInsert (fxCreator fixture))
    underwritingId <- insertItem (underwritingInsert (fxCreator fixture) (fxUnderwriter fixture))
    promoId <- insertItem (promoInsert (fxCreator fixture) (fxShow fixture) UUT.Pending)
    stored <- TRX.statement () (UUT.getByIdsIncludingDeleted [psaId, announcementId, underwritingId, promoId])
    pure (map UUT.bimCategory stored)
  categories `shouldBe` [UUT.Psa, UUT.Announcement, UUT.Underwriting, UUT.ShowPromo]

refusesPromoWithoutShow :: TestDBConfig -> IO ()
refusesPromoWithoutShow cfg =
  expectRefused cfg $ do
    fixture <- setup
    let promo = promoInsert (fxCreator fixture) (fxShow fixture) UUT.Pending
    insertItem promo {UUT.biiShowId = Nothing}

refusesPsaWithShow :: TestDBConfig -> IO ()
refusesPsaWithShow cfg =
  expectRefused cfg $ do
    fixture <- setup
    insertItem (psaInsert (fxCreator fixture)) {UUT.biiShowId = Just (fxShow fixture)}

refusesAnnouncementWithUnderwriter :: TestDBConfig -> IO ()
refusesAnnouncementWithUnderwriter cfg =
  expectRefused cfg $ do
    fixture <- setup
    let announcement = announcementInsert (fxCreator fixture)
    insertItem announcement {UUT.biiUnderwriterId = Just (fxUnderwriter fixture)}

refusesUnderwritingWithShow :: TestDBConfig -> IO ()
refusesUnderwritingWithShow cfg =
  expectRefused cfg $ do
    fixture <- setup
    let underwriting = underwritingInsert (fxCreator fixture) (fxUnderwriter fixture)
    insertItem underwriting {UUT.biiShowId = Just (fxShow fixture)}

--------------------------------------------------------------------------------
-- The Review Constraints

refusesPendingPsa :: TestDBConfig -> IO ()
refusesPendingPsa cfg =
  expectRefused cfg $ do
    fixture <- setup
    insertItem (psaInsert (fxCreator fixture)) {UUT.biiReviewStatus = UUT.Pending}

-- | No query writes a note on an approved promo, so this case writes it with
-- plain SQL.
refusesNoteOnApprovedPromo :: TestDBConfig -> IO ()
refusesNoteOnApprovedPromo cfg =
  expectRefused cfg $ do
    fixture <- setup
    promoId <- insertItem (promoInsert (fxCreator fixture) (fxShow fixture) UUT.Approved)
    setReviewNote promoId "Too loud."

--------------------------------------------------------------------------------
-- getActiveInMonth

includesApprovedPromo :: TestDBConfig -> IO ()
includesApprovedPromo cfg = do
  (promoId, ids) <- runOrFail cfg $ do
    fixture <- setup
    promoId <- insertItem (promoInsert (fxCreator fixture) (fxShow fixture) UUT.Approved)
    ids <- activeIds
    pure (promoId, ids)
  ids `shouldBe` [promoId]

excludesPendingPromo :: TestDBConfig -> IO ()
excludesPendingPromo cfg = do
  ids <- runOrFail cfg $ do
    fixture <- setup
    _ <- insertItem (promoInsert (fxCreator fixture) (fxShow fixture) UUT.Pending)
    activeIds
  ids `shouldBe` []

excludesRejectedPromo :: TestDBConfig -> IO ()
excludesRejectedPromo cfg = do
  ids <- runOrFail cfg $ do
    fixture <- setup
    promoId <- insertItem (promoInsert (fxCreator fixture) (fxShow fixture) UUT.Pending)
    _ <- TRX.statement () (UUT.rejectPromo promoId "The audio clips at the end.")
    activeIds
  ids `shouldBe` []

excludesInactiveShowPromo :: TestDBConfig -> IO ()
excludesInactiveShowPromo cfg = do
  ids <- runOrFail cfg $ do
    fixture <- setup
    inactiveShowId <- insertShow "summer-sessions" Shows.Inactive
    _ <- insertItem (promoInsert (fxCreator fixture) inactiveShowId UUT.Approved)
    activeIds
  ids `shouldBe` []

excludesDeletedShowPromo :: TestDBConfig -> IO ()
excludesDeletedShowPromo cfg = do
  ids <- runOrFail cfg $ do
    fixture <- setup
    _ <- insertItem (promoInsert (fxCreator fixture) (fxShow fixture) UUT.Approved)
    deleteShow (fxShow fixture)
    activeIds
  ids `shouldBe` []

includesItemsWithoutShow :: TestDBConfig -> IO ()
includesItemsWithoutShow cfg = do
  (expected, ids) <- runOrFail cfg $ do
    fixture <- setup
    psaId <- insertItem (psaInsert (fxCreator fixture))
    announcementId <- insertItem (announcementInsert (fxCreator fixture))
    ids <- activeIds
    pure ([psaId, announcementId], ids)
  ids `shouldBe` expected

--------------------------------------------------------------------------------
-- approvePromo and rejectPromo

approvesPending :: TestDBConfig -> IO ()
approvesPending cfg = do
  mApproved <- runOrFail cfg $ do
    fixture <- setup
    promoId <- insertItem (promoInsert (fxCreator fixture) (fxShow fixture) UUT.Pending)
    TRX.statement () (UUT.approvePromo promoId)
  fmap UUT.bimReviewStatus mApproved `shouldBe` Just UUT.Approved
  fmap UUT.bimReviewNote mApproved `shouldBe` Just Nothing

rejectsPending :: TestDBConfig -> IO ()
rejectsPending cfg = do
  mRejected <- runOrFail cfg $ do
    fixture <- setup
    promoId <- insertItem (promoInsert (fxCreator fixture) (fxShow fixture) UUT.Pending)
    TRX.statement () (UUT.rejectPromo promoId "Say the air time in the promo.")
  fmap UUT.bimReviewStatus mRejected `shouldBe` Just UUT.Rejected
  fmap UUT.bimReviewNote mRejected `shouldBe` Just (Just "Say the air time in the promo.")

approveTwiceDoesNothing :: TestDBConfig -> IO ()
approveTwiceDoesNothing cfg = do
  mSecond <- runOrFail cfg $ do
    fixture <- setup
    promoId <- insertItem (promoInsert (fxCreator fixture) (fxShow fixture) UUT.Pending)
    _ <- TRX.statement () (UUT.approvePromo promoId)
    TRX.statement () (UUT.approvePromo promoId)
  mSecond `shouldBe` Nothing

rejectApprovedDoesNothing :: TestDBConfig -> IO ()
rejectApprovedDoesNothing cfg = do
  (mRejected, mStored) <- runOrFail cfg $ do
    fixture <- setup
    promoId <- insertItem (promoInsert (fxCreator fixture) (fxShow fixture) UUT.Approved)
    mRejected <- TRX.statement () (UUT.rejectPromo promoId "Too late.")
    mStored <- TRX.statement () (UUT.getById promoId)
    pure (mRejected, mStored)
  mRejected `shouldBe` Nothing
  fmap UUT.bimReviewStatus mStored `shouldBe` Just UUT.Approved

approveRejectedDoesNothing :: TestDBConfig -> IO ()
approveRejectedDoesNothing cfg = do
  mApproved <- runOrFail cfg $ do
    fixture <- setup
    promoId <- insertItem (promoInsert (fxCreator fixture) (fxShow fixture) UUT.Pending)
    _ <- TRX.statement () (UUT.rejectPromo promoId "The audio clips at the end.")
    TRX.statement () (UUT.approvePromo promoId)
  mApproved `shouldBe` Nothing

approvePsaDoesNothing :: TestDBConfig -> IO ()
approvePsaDoesNothing cfg = do
  mApproved <- runOrFail cfg $ do
    fixture <- setup
    psaId <- insertItem (psaInsert (fxCreator fixture))
    TRX.statement () (UUT.approvePromo psaId)
  mApproved `shouldBe` Nothing

approveDeletedDoesNothing :: TestDBConfig -> IO ()
approveDeletedDoesNothing cfg = do
  mApproved <- runOrFail cfg $ do
    fixture <- setup
    promoId <- insertItem (promoInsert (fxCreator fixture) (fxShow fixture) UUT.Pending)
    _ <- TRX.statement () (UUT.softDeleteBreakItem promoId)
    TRX.statement () (UUT.approvePromo promoId)
  mApproved `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Promo Lists
--
-- Each case inserts its rows in one transaction, so every row has the same
-- created_at. The ID breaks the tie, so the later insert counts as newer.

promosForShow :: TestDBConfig -> IO ()
promosForShow cfg = do
  (expected, listed) <- runOrFail cfg $ do
    fixture <- setup
    otherShowId <- insertShow "morning-glory" Shows.Active
    let creatorId = fxCreator fixture
        showId = fxShow fixture
    olderId <- insertItem (promoInsert creatorId showId UUT.Approved)
    newerId <- insertItem (promoInsert creatorId showId UUT.Pending)
    deletedId <- insertItem (promoInsert creatorId showId UUT.Pending)
    _ <- TRX.statement () (UUT.softDeleteBreakItem deletedId)
    _ <- insertItem (promoInsert creatorId otherShowId UUT.Pending)
    _ <- insertItem (psaInsert creatorId)
    listed <- TRX.statement () (UUT.getPromosForShow showId)
    pure ([newerId, olderId], map UUT.bimId listed)
  listed `shouldBe` expected

pendingPromos :: TestDBConfig -> IO ()
pendingPromos cfg = do
  (expected, listed) <- runOrFail cfg $ do
    fixture <- setup
    otherShowId <- insertShow "morning-glory" Shows.Active
    let creatorId = fxCreator fixture
        showId = fxShow fixture
    olderId <- insertItem (promoInsert creatorId showId UUT.Pending)
    newerId <- insertItem (promoInsert creatorId otherShowId UUT.Pending)
    _ <- insertItem (promoInsert creatorId showId UUT.Approved)
    deletedId <- insertItem (promoInsert creatorId showId UUT.Pending)
    _ <- TRX.statement () (UUT.softDeleteBreakItem deletedId)
    listed <- TRX.statement () UUT.getPendingPromos
    pure ([olderId, newerId], map UUT.bimId listed)
  listed `shouldBe` expected

-- | Approved and rejected promos from two shows come newest first. A pending
-- promo, a deleted promo, and a PSA are left out.
reviewedPromos :: TestDBConfig -> IO ()
reviewedPromos cfg = do
  (expected, listed) <- runOrFail cfg $ do
    fixture <- setup
    otherShowId <- insertShow "morning-glory" Shows.Active
    let creatorId = fxCreator fixture
        showId = fxShow fixture
    olderApprovedId <- insertItem (promoInsert creatorId showId UUT.Approved)
    _ <- insertItem (promoInsert creatorId showId UUT.Pending)
    rejectedId <- insertItem (promoInsert creatorId otherShowId UUT.Pending)
    _ <- TRX.statement () (UUT.rejectPromo rejectedId "The audio clips at the end.")
    newerApprovedId <- insertItem (promoInsert creatorId otherShowId UUT.Approved)
    deletedId <- insertItem (promoInsert creatorId showId UUT.Approved)
    _ <- TRX.statement () (UUT.softDeleteBreakItem deletedId)
    _ <- insertItem (psaInsert creatorId)
    listed <- TRX.statement () (UUT.getReviewedPromos 20 0)
    pure ([newerApprovedId, rejectedId, olderApprovedId], map UUT.bimId listed)
  listed `shouldBe` expected
