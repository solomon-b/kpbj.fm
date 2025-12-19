module API.Dashboard.Episodes.Get.Templates.Page
  ( template,
  )
where

import API.Dashboard.Get.Templates.Episode (renderEpisodeTableRow)
import Component.Table (ColumnAlign (..), ColumnHeader (..), TableConfig (..), renderTable)
import Design.StyleBuilder.Internal (cls)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

-- | Episodes dashboard template (stats are now in the top bar)
template ::
  UserMetadata.Model ->
  Maybe Shows.Model ->
  [Episodes.Model] ->
  Lucid.Html ()
template = renderEpisodesSection

-- | Episodes table section
renderEpisodesSection :: UserMetadata.Model -> Maybe Shows.Model -> [Episodes.Model] -> Lucid.Html ()
renderEpisodesSection userMeta selectedShow episodes =
  Lucid.section_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6]] $ do
    case episodes of
      [] ->
        Lucid.div_ [Lucid.class_ $ cls [Tokens.textGray600, "text-center", "p-8"]] $ do
          Lucid.p_ "No episodes uploaded yet."
          Lucid.p_ [Lucid.class_ $ cls [Tokens.textSm, "mt-2"]] "Use 'New Episode' to upload your first episode."
      _ ->
        renderTable
          TableConfig
            { headers =
                [ ColumnHeader "#" AlignLeft,
                  ColumnHeader "TITLE" AlignLeft,
                  ColumnHeader "SCHEDULED" AlignLeft,
                  ColumnHeader "STATUS" AlignLeft,
                  ColumnHeader "ACTIONS" AlignRight
                ],
              wrapperClass = "overflow-x-auto",
              tableClass = "w-full"
            }
          $ mapM_ (maybe (const mempty) (renderEpisodeTableRow userMeta) selectedShow) episodes
