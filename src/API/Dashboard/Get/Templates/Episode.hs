{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Get.Templates.Episode
  ( renderEpisodeTableRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEpisodesLinks, showEpisodesLinks)
import API.Types
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Base qualified as LucidBase
import Lucid.Extras (hxDelete_, hxGet_, hxPost_, hxPushUrl_, hxSwap_, hxTarget_, xData_, xOnChange_, xRef_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

episodeGetUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeGetUrl showSlug epNum = Links.linkURI $ dashboardEpisodesLinks.detail showSlug epNum

episodeEditGetUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeEditGetUrl showSlug epNum = Links.linkURI $ dashboardEpisodesLinks.editGet showSlug epNum

episodeArchiveUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeArchiveUrl showSlug epNum = Links.linkURI $ showEpisodesLinks.delete showSlug epNum

episodeDiscardDraftUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeDiscardDraftUrl showSlug epNum = Links.linkURI $ showEpisodesLinks.discardDraft showSlug epNum

episodePublishUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodePublishUrl showSlug epNum = Links.linkURI $ showEpisodesLinks.publish showSlug epNum

--------------------------------------------------------------------------------

-- | Render individual episode as table row.
--
-- Actions shown depend on episode status and user role:
-- - Draft episodes: hosts can "Discard Draft" (hard delete)
-- - Published episodes: only staff+ can "Archive" (soft delete)
renderEpisodeTableRow :: UserMetadata.Model -> Shows.Model -> Episodes.Model -> Lucid.Html ()
renderEpisodeTableRow userMeta showModel episode = do
  let episodeId = episode.id
      episodeRowId = [i|episode-row-#{episodeId}|]
  Lucid.tr_ [class_ $ base ["border-b", "border-gray-300", "hover:bg-gray-50"], Lucid.id_ episodeRowId] $ do
    -- Episode number and description
    Lucid.td_ [class_ $ base [Tokens.px4, "py-3"]] $ do
      let episodeUrl = episodeGetUrl showModel.slug epNum
      Lucid.div_ [class_ $ base [Tokens.fontBold, "text-gray-900"]]
        $ Lucid.a_
          [ Lucid.href_ [i|/#{episodeUrl}|],
            hxGet_ [i|/#{episodeUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["hover:underline", "text-blue-600"]
          ]
        $ Lucid.toHtml ("#" <> show epNum)
      case episode.description of
        Nothing -> mempty
        Just desc ->
          Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.textGray600, "mt-1"]] $ do
            Lucid.toHtml $ Text.take 100 desc
            if Text.length desc > 100 then "..." else ""

    -- Scheduled date
    Lucid.td_ [class_ $ base [Tokens.px4, "py-3", Tokens.textSm]] $ do
      case episode.scheduledAt of
        Just scheduledAt -> Lucid.toHtml $ Text.pack $ formatTime defaultTimeLocale "%b %d, %Y" scheduledAt
        Nothing -> Lucid.span_ [class_ $ base ["text-gray-500", "italic"]] "Not scheduled"

    -- Status
    Lucid.td_ [class_ $ base [Tokens.px4, "py-3", Tokens.textSm]] $ do
      case episode.status of
        Episodes.Draft ->
          Lucid.span_ [class_ $ base ["inline-block", "bg-yellow-100", "text-yellow-800", "px-2", "py-1", "rounded", Tokens.textXs, Tokens.fontBold]] "DRAFT"
        Episodes.Published ->
          Lucid.span_ [class_ $ base ["inline-block", "bg-green-100", "text-green-800", "px-2", "py-1", "rounded", Tokens.textXs, Tokens.fontBold]] "PUBLISHED"
        Episodes.Deleted ->
          Lucid.span_ [class_ $ base ["inline-block", "bg-red-100", "text-red-800", "px-2", "py-1", "rounded", Tokens.textXs, Tokens.fontBold]] "DELETED"

    -- Actions dropdown
    Lucid.td_ [class_ $ base [Tokens.px4, "py-3", "text-center"]] $
      Lucid.div_ [xData_ "{}"] $
        do
          -- Hidden link for Edit
          Lucid.a_
            [ Lucid.href_ [i|/#{episodeEditUrl}|],
              hxGet_ [i|/#{episodeEditUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              xRef_ "editLink",
              Lucid.class_ "hidden"
            ]
            ""
          -- Hidden button for Discard Draft (hard delete for drafts)
          Lucid.button_
            [ hxDelete_ [i|/#{discardDraftUrl}|],
              hxTarget_ ("#" <> episodeRowId),
              hxSwap_ "outerHTML",
              LucidBase.makeAttributes "hx-confirm" "Are you sure you want to discard this draft? This action cannot be undone.",
              xRef_ "discardBtn",
              Lucid.class_ "hidden"
            ]
            ""
          -- Hidden button for Archive (soft delete for staff+)
          Lucid.button_
            [ hxDelete_ [i|/#{archiveUrl}|],
              hxTarget_ ("#" <> episodeRowId),
              hxSwap_ "outerHTML",
              LucidBase.makeAttributes "hx-confirm" "Are you sure you want to archive this episode? It will no longer appear publicly.",
              xRef_ "archiveBtn",
              Lucid.class_ "hidden"
            ]
            ""
          -- Hidden button for Publish
          Lucid.button_
            [ hxPost_ [i|/#{episodePubUrl}|],
              hxTarget_ ("#" <> episodeRowId),
              hxSwap_ "outerHTML",
              LucidBase.makeAttributes "hx-confirm" "Are you sure you want to publish this episode? It will become publicly visible.",
              xRef_ "publishBtn",
              Lucid.class_ "hidden"
            ]
            ""
          -- Visible dropdown
          Lucid.select_
            [ Lucid.class_ "p-2 border border-gray-400 text-xs bg-white",
              xOnChange_
                [i|
              const action = $el.value;
              $el.value = '';
              if (action === 'edit') {
                $refs.editLink.click();
              } else if (action === 'discard') {
                $refs.discardBtn.click();
              } else if (action === 'archive') {
                $refs.archiveBtn.click();
              } else if (action === 'publish') {
                $refs.publishBtn.click();
              }
            |]
            ]
            $ do
              Lucid.option_ [Lucid.value_ ""] "Actions..."
              Lucid.option_ [Lucid.value_ "edit"] "Edit"
              -- Only show Publish option for draft episodes
              case episode.status of
                Episodes.Draft -> do
                  Lucid.option_ [Lucid.value_ "publish"] "Publish"
                  -- Hosts can discard their own drafts
                  Lucid.option_ [Lucid.value_ "discard"] "Discard Draft"
                _ -> mempty
              -- Staff+ can archive any episode (soft delete)
              if isStaff
                then Lucid.option_ [Lucid.value_ "archive"] "Archive"
                else mempty
  where
    epNum = episode.episodeNumber
    episodeEditUrl = episodeEditGetUrl showModel.slug epNum
    discardDraftUrl = episodeDiscardDraftUrl showModel.slug epNum
    archiveUrl = episodeArchiveUrl showModel.slug epNum
    episodePubUrl = episodePublishUrl showModel.slug epNum
    isStaff = UserMetadata.isStaffOrHigher userMeta.mUserRole
