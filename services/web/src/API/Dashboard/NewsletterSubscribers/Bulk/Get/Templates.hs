{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.NewsletterSubscribers.Bulk.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardNewsletterSubscribersLinks)
import API.Types
import Data.String.Interpolate (i)
import Data.Text (Text)
import Lucid qualified
import Lucid.Form.Builder
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Bulk add subscribers form template.
template :: Lucid.Html ()
template =
  renderForm config form
  where
    listUri :: Links.URI
    listUri = Links.linkURI $ dashboardNewsletterSubscribersLinks.list Nothing Nothing

    bulkPostUri :: Links.URI
    bulkPostUri = Links.linkURI dashboardNewsletterSubscribersLinks.bulkPost

    listUrl :: Text
    listUrl = [i|/#{listUri}|]

    postUrl :: Text
    postUrl = [i|/#{bulkPostUri}|]

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = postUrl,
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content"
        }

    form :: FormBuilder
    form = do
      formTitle "BULK ADD SUBSCRIBERS"
      formSubtitle "Paste one email per line, or comma-separated."

      textareaField "emails" 12 $ do
        label "Email Addresses"
        placeholder "alice@example.com\nbob@example.com, carol@example.com"
        hint "Duplicates are ignored. Invalid addresses are skipped and reported."
        required

      cancelButton listUrl "CANCEL"
      submitButton "ADD SUBSCRIBERS"
