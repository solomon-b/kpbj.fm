{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The underwriters page.
--
-- One list. An add form sits above the table, and each row holds a rename form.
-- Both forms post with HTMX and replace the table body with the new list.
module API.Dashboard.Underwriters.Get.Templates.Page
  ( template,
    renderTableBody,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardUnderwritersLinks)
import API.Types (DashboardUnderwritersRoutes (..))
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    renderIndexTable,
  )
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | DOM id of the table body that both forms replace.
tableBodyId :: Text
tableBodyId = "underwriters-table-body"

-- | The whole page.
template :: [Underwriters.Model] -> Lucid.Html ()
template underwriters =
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $ do
    renderAddForm
    renderIndexTable
      IndexTableConfig
        { itcBodyId = tableBodyId,
          itcHeaders =
            [ ColumnHeader "Name" AlignLeft,
              ColumnHeader "Id" AlignLeft
            ],
          itcNextPageUrl = Nothing,
          itcPaginationConfig = Nothing
        }
      (renderTableBody underwriters)

-- | The rows alone. The POST routes return this.
renderTableBody :: [Underwriters.Model] -> Lucid.Html ()
renderTableBody [] =
  Lucid.tr_ $
    Lucid.td_
      [Lucid.colspan_ "2", class_ $ base [Tokens.p4, Tokens.textSm, Tokens.fgMuted]]
      "No underwriters yet. Add one above."
renderTableBody underwriters = mapM_ renderRow underwriters

-- | The add form.
--
-- The name input is required, so the browser stops an empty name before the
-- POST. A failed POST returns no rows, which would clear the table.
renderAddForm :: Lucid.Html ()
renderAddForm =
  Lucid.form_
    [ hxPost_ [i|/#{newPostUri}|],
      hxTarget_ ("#" <> tableBodyId),
      hxSwap_ "innerHTML",
      hxOnAfterRequest_ "if(event.detail.successful) this.reset()",
      class_ $ base ["flex", Tokens.gap4, Tokens.p4]
    ]
    $ do
      nameInput ""
      saveButton "ADD UNDERWRITER"
  where
    newPostUri = Links.linkURI dashboardUnderwritersLinks.newPost

-- | One row, which is also the rename form.
renderRow :: Underwriters.Model -> Lucid.Html ()
renderRow u =
  Lucid.tr_ [class_ $ base ["border-b-2", Tokens.borderMuted]] $ do
    Lucid.td_ [class_ $ base [Tokens.p4]]
      $ Lucid.form_
        [ hxPost_ [i|/#{editPostUri}|],
          hxTarget_ ("#" <> tableBodyId),
          hxSwap_ "innerHTML",
          class_ $ base ["flex", Tokens.gap4]
        ]
      $ do
        nameInput u.uwName
        saveButton "RENAME"
    Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm, Tokens.fgMuted]] $
      Lucid.toHtml (display u.uwId)
  where
    editPostUri = Links.linkURI (dashboardUnderwritersLinks.editPost u.uwId)

nameInput :: Text -> Lucid.Html ()
nameInput current =
  Lucid.input_
    [ Lucid.type_ "text",
      Lucid.name_ "name",
      Lucid.value_ current,
      Lucid.placeholder_ "Underwriter name",
      Lucid.required_ "required",
      Lucid.maxlength_ "200",
      class_ $ base ["flex-1", Tokens.px3, "py-1", Tokens.textSm, Tokens.bgAlt, Tokens.fgPrimary, "border", Tokens.borderMuted]
    ]

saveButton :: Text -> Lucid.Html ()
saveButton =
  Lucid.button_
    [ Lucid.type_ "submit",
      class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, Tokens.bgAlt, Tokens.fgPrimary, "border", Tokens.borderMuted, "hover:opacity-80"]
    ]
    . Lucid.toHtml
