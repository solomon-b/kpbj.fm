{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.NewsletterSubscribers.Bulk.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.NewsletterSubscribers.Bulk.Post.Route (BulkForm (..))
import API.Links (dashboardNewsletterSubscribersLinks)
import API.Types
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.EmailAddress (EmailAddress, isValid, mkEmailAddress)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Effects.Mailchimp.Sync (SyncOp (..), syncAsync)
import Log qualified
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Handler for POST /dashboard/newsletter-subscribers/bulk.
handler ::
  Maybe Cookie ->
  BulkForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler cookie form =
  handleRedirectErrors "Newsletter subscribers bulk add" dashboardNewsletterSubscribersLinks.bulkGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can add newsletter subscribers." userMetadata

    let (valid, invalidCount) = parseEmails form.bfEmails

    (added, duplicate) <- foldM insertOne (0 :: Int, 0 :: Int) valid

    Log.logInfo "Bulk added newsletter subscribers" $
      Text.pack $
        show (added, duplicate, invalidCount)

    let bannerType = if added > 0 then Success else Info
        flash = FlashMessage bannerType "Bulk Add Complete" (summary added duplicate invalidCount)

    pure $
      Servant.addHeader listUrl $
        Servant.addHeader (flashCookie (Just flash)) Servant.NoContent
  where
    listUrl :: Text
    listUrl =
      let uri = Links.linkURI $ dashboardNewsletterSubscribersLinks.list Nothing Nothing
       in [i|/#{uri}|]

    insertOne (added, duplicate) email = do
      result <- execQuery (NewsletterSubscribers.insert (NewsletterSubscribers.Insert email))
      case result of
        Left dbErr -> throwDatabaseError dbErr
        Right (Just subId) -> do
          lift $ syncAsync (Upsert email subId)
          pure (added + 1, duplicate)
        Right Nothing -> pure (added, duplicate + 1)

-- | Split, trim, dedupe, and validate the pasted email list.
--
-- Returns the unique valid 'EmailAddress' values and a count of invalid tokens.
parseEmails :: Text -> ([EmailAddress], Int)
parseEmails raw =
  let tokens =
        filter (not . Text.null) $
          map Text.strip $
            concatMap (Text.splitOn ",") $
              Text.lines raw
      uniqueTokens = dedupe tokens
      (valid, invalid) =
        foldl'
          ( \(vs, is) tok ->
              let candidate = mkEmailAddress tok
               in if isValid candidate
                    then (candidate : vs, is)
                    else (vs, is + 1)
          )
          ([], 0 :: Int)
          uniqueTokens
   in (reverse valid, invalid)

-- | Drop duplicate tokens while preserving the first-seen order.
dedupe :: [Text] -> [Text]
dedupe = go Set.empty
  where
    go _ [] = []
    go seen (t : ts)
      | Set.member t seen = go seen ts
      | otherwise = t : go (Set.insert t seen) ts

-- | Build the user-facing summary string, omitting zero-count phrases.
summary :: Int -> Int -> Int -> Text
summary added duplicate invalid =
  let parts =
        concat
          [ [Text.pack (show added) <> " added"],
            [Text.pack (show duplicate) <> " already subscribed" | duplicate > 0],
            [Text.pack (show invalid) <> " invalid" | invalid > 0]
          ]
   in Text.intercalate ", " parts <> "."
