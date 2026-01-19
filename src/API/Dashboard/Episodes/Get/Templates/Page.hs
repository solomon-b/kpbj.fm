{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Episodes.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.EpisodeRow (renderEpisodeTableRow)
import API.Links (dashboardEpisodesLinks)
import API.Types
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    renderIndexTable,
  )
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Episodes dashboard template (stats are now in the top bar)
template ::
  UserMetadata.Model ->
  Maybe Shows.Model ->
  [Episodes.Model] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
template = renderEpisodesSection

-- | Episodes table section
renderEpisodesSection :: UserMetadata.Model -> Maybe Shows.Model -> [Episodes.Model] -> Int64 -> Bool -> Lucid.Html ()
renderEpisodesSection userMeta selectedShow episodes currentPage hasMore = do
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "overflow-hidden"]] $
    case (episodes, selectedShow) of
      ([], _) ->
        renderEmptyState
      (_, Nothing) ->
        mempty
      (_, Just showModel) ->
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "episodes-table-body",
              itcHeaders =
                [ ColumnHeader "EPISODE" AlignLeft,
                  ColumnHeader "SCHEDULED" AlignLeft,
                  ColumnHeader "STATUS" AlignLeft,
                  ColumnHeader "ACTIONS" AlignRight
                ],
              itcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl showModel}|] else Nothing,
              itcPaginationConfig =
                Just
                  PaginationConfig
                    { pcPrevPageUrl = if currentPage > 1 then Just [i|/#{prevPageUrl showModel}|] else Nothing,
                      pcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl showModel}|] else Nothing,
                      pcCurrentPage = currentPage
                    }
            }
          (mapM_ (renderEpisodeTableRow userMeta showModel) episodes)
  where
    nextPageUrl :: Shows.Model -> Links.URI
    nextPageUrl showModel = Links.linkURI $ dashboardEpisodesLinks.list showModel.slug (Just (currentPage + 1))
    prevPageUrl :: Shows.Model -> Links.URI
    prevPageUrl showModel = Links.linkURI $ dashboardEpisodesLinks.list showModel.slug (Just (currentPage - 1))

-- | Empty state when no episodes exist
renderEmptyState :: Lucid.Html ()
renderEmptyState =
  Lucid.div_ [class_ $ base [Tokens.textGray600, "text-center", "p-8"]] $ do
    Lucid.p_ "No episodes uploaded yet."
    Lucid.p_ [class_ $ base [Tokens.textSm, "mt-2"]] "Use 'New Episode' to upload your first episode."
