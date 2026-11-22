{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Episodes.Get.Templates.EpisodeRow
  ( renderEpisodeTableRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEpisodesLinks)
import API.Types (DashboardEpisodesRoutes (..))
import Component.Button ((&))
import Component.Button qualified as Button
import Control.Monad (unless, when)
import Data.Maybe (isJust, isNothing)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.Timezone (formatDate)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Markdown (markdownToPlainText)
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

episodeGetUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeGetUrl showSlug epNum = Links.linkURI $ dashboardEpisodesLinks.detail showSlug epNum

episodeEditGetUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeEditGetUrl showSlug epNum = Links.linkURI $ dashboardEpisodesLinks.editGet showSlug epNum

episodeArchiveUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeArchiveUrl showSlug epNum = Links.linkURI $ dashboardEpisodesLinks.delete showSlug epNum

episodeUnarchiveUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeUnarchiveUrl showSlug epNum = Links.linkURI $ dashboardEpisodesLinks.unarchive showSlug epNum

--------------------------------------------------------------------------------

-- | Render individual episode as table row.
--
-- Actions shown depend on user role:
-- - Staff+ can "Archive" (soft delete via deleted_at)
-- - Staff+ can "Unarchive" an already archived episode
-- - All users can "Edit"
--
-- Only staff and admins receive an archived episode, because the list query
-- gives a host the live rows only. An archived row is dimmed and carries an
-- ARCHIVED badge, and it offers Unarchive in place of Archive and Edit.
renderEpisodeTableRow :: UserMetadata.Model -> Shows.Model -> Episodes.Model -> Lucid.Html ()
renderEpisodeTableRow userMeta showModel episode = do
  let episodeId = episode.id
      episodeRowId = [i|episode-row-#{episodeId}|]
      detailUrl = episodeGetUrl showModel.slug epNum
      isArchived = isJust episode.deletedAt
      cellLinkAttrs =
        [ class_ $ base [Tokens.p4, "cursor-pointer"],
          hxGet_ [i|/#{detailUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
  Lucid.tr_ [class_ $ base $ ["border-b-2", Tokens.borderMuted, Tokens.hoverBg] <> archivedRowClasses, Lucid.id_ episodeRowId] $ do
    -- Episode number and description
    Lucid.td_ cellLinkAttrs $ do
      Lucid.span_ [class_ $ base [Tokens.fontBold]] $
        Lucid.toHtml ("#" <> show epNum)
      case episode.description of
        Nothing -> mempty
        Just desc -> do
          -- Strip Markdown before the truncation, so the snippet is plain text.
          let plainDesc = markdownToPlainText desc
          Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, "mt-1"]] $ do
            Lucid.toHtml $ Text.take 100 plainDesc
            if Text.length plainDesc > 100 then "..." else ""

    -- The date the episode airs on
    Lucid.td_ cellLinkAttrs $
      case episode.airDate of
        Nothing -> mempty
        Just airDate -> Lucid.toHtml $ formatDate airDate

    -- Status column
    Lucid.td_ cellLinkAttrs $
      if isArchived
        then Lucid.span_ [class_ $ base ["inline-block", Tokens.errorBg, Tokens.errorText, "px-2", "py-1", "rounded", Tokens.textXs, Tokens.fontBold]] "ARCHIVED"
        else
          if isNothing episode.scheduleTemplateId
            then Lucid.span_ [class_ $ base ["inline-block", Tokens.warningBg, Tokens.warningText, "px-2", "py-1", "rounded", Tokens.textXs, Tokens.fontBold]] "UNSCHEDULED"
            else mempty

    -- Actions
    Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm, "text-center"]] $
      Button.group $ do
        -- An archived episode is read-only. Unarchive it to edit it.
        unless isArchived $
          Button.render (Button.defaults & Button.dashboardLink [i|/#{episodeEditUrl}|]) "Edit"
        -- Staff+ can archive any episode (soft delete)
        when (isStaff && not isArchived) $
          Button.render
            ( Button.defaults
                & Button.danger
                & Button.with
                  [ hxDelete_ [i|/#{archiveUrl}|],
                    hxTarget_ ("#" <> episodeRowId),
                    hxSwap_ "outerHTML",
                    hxConfirm_ "Are you sure you want to archive this episode? It will no longer appear publicly."
                  ]
            )
            "Archive"
        -- Staff+ can put an archived episode back on the public site
        when (isStaff && isArchived) $
          Button.render
            ( Button.defaults
                & Button.with
                  [ hxPost_ [i|/#{unarchiveUrl}|],
                    hxTarget_ ("#" <> episodeRowId),
                    hxSwap_ "outerHTML",
                    hxConfirm_ "Unarchive this episode? It will appear publicly again."
                  ]
            )
            "Unarchive"
  where
    epNum = episode.episodeNumber
    episodeEditUrl = episodeEditGetUrl showModel.slug epNum
    archiveUrl = episodeArchiveUrl showModel.slug epNum
    unarchiveUrl = episodeUnarchiveUrl showModel.slug epNum
    isStaff = UserMetadata.isStaffOrHigher userMeta.mUserRole
    -- Dim an archived row, so the badge is not the only signal.
    archivedRowClasses = ["opacity-60" | isJust episode.deletedAt]
