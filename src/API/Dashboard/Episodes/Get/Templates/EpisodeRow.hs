{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Episodes.Get.Templates.EpisodeRow
  ( renderEpisodeTableRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEpisodesLinks)
import API.Types (DashboardEpisodesRoutes (..))
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.Timezone (formatPacificDate)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Alpine
import Lucid.Base qualified as LucidBase
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

episodeGetUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeGetUrl showSlug epNum = Links.linkURI $ dashboardEpisodesLinks.detail showSlug epNum

episodeEditGetUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeEditGetUrl showSlug epNum = Links.linkURI $ dashboardEpisodesLinks.editGet showSlug epNum

episodeArchiveUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeArchiveUrl showSlug epNum = Links.linkURI $ dashboardEpisodesLinks.delete showSlug epNum

--------------------------------------------------------------------------------

-- | Render individual episode as table row.
--
-- Actions shown depend on user role:
-- - Staff+ can "Archive" (soft delete via deleted_at)
-- - All users can "Edit"
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
  Lucid.tr_ [class_ $ base ["border-b-2", Tokens.borderMuted, Tokens.hoverBg], Lucid.id_ episodeRowId] $ do
    -- Episode number and description
    Lucid.td_ cellLinkAttrs $ do
      Lucid.span_ [class_ $ base [Tokens.fontBold]] $
        Lucid.toHtml ("#" <> show epNum)
      case episode.description of
        Nothing -> mempty
        Just desc ->
          Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, "mt-1"]] $ do
            Lucid.toHtml $ Text.take 100 desc
            if Text.length desc > 100 then "..." else ""

    -- Scheduled date (converted to Pacific time)
    Lucid.td_ cellLinkAttrs $
      Lucid.toHtml $
        formatPacificDate episode.scheduledAt

    -- Status column - only show badge if archived
    Lucid.td_ cellLinkAttrs $
      if isArchived
        then Lucid.span_ [class_ $ base ["inline-block", Tokens.errorBg, Tokens.errorText, "px-2", "py-1", "rounded", Tokens.textXs, Tokens.fontBold]] "ARCHIVED"
        else mempty

    -- Actions dropdown
    Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
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
          -- Visible dropdown
          Lucid.select_
            [ class_ $ base ["p-2", "border", Tokens.borderMuted, "text-xs", Tokens.bgMain, Tokens.fgPrimary],
              xOnChange_
                [i|
              const action = $el.value;
              $el.value = '';
              if (action === 'edit') {
                $refs.editLink.click();
              } else if (action === 'archive') {
                $refs.archiveBtn.click();
              }
            |]
            ]
            $ do
              Lucid.option_ [Lucid.value_ ""] "Actions..."
              Lucid.option_ [Lucid.value_ "edit"] "Edit"
              -- Staff+ can archive any episode (soft delete)
              if isStaff && not isArchived
                then Lucid.option_ [Lucid.value_ "archive"] "Archive"
                else mempty
  where
    epNum = episode.episodeNumber
    episodeEditUrl = episodeEditGetUrl showModel.slug epNum
    archiveUrl = episodeArchiveUrl showModel.slug epNum
    isStaff = UserMetadata.isStaffOrHigher userMeta.mUserRole
