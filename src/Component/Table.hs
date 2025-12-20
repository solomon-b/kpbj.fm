{-# LANGUAGE OverloadedRecordDot #-}

module Component.Table
  ( renderTable,
    TableConfig (..),
    ColumnHeader (..),
    ColumnAlign (..),
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified

--------------------------------------------------------------------------------

-- | Column alignment options
data ColumnAlign = AlignLeft | AlignRight | AlignCenter
  deriving (Show, Eq)

-- | Column header with optional alignment
data ColumnHeader = ColumnHeader
  { headerText :: Text,
    headerAlign :: ColumnAlign
  }
  deriving (Show, Eq)

-- | Configuration for table rendering
data TableConfig = TableConfig
  { headers :: [ColumnHeader],
    -- | Optional CSS classes to add to the table wrapper
    wrapperClass :: Text,
    -- | Optional CSS classes to add to the table element
    tableClass :: Text
  }

-- | Render a table with headers and body content
--
-- Example usage:
-- @
-- renderTable
--   TableConfig
--     { headers =
--         [ ColumnHeader "Name" AlignLeft,
--           ColumnHeader "Age" AlignRight,
--           ColumnHeader "Email" AlignLeft
--         ],
--       wrapperClass = "overflow-x-auto",
--       tableClass = "w-full"
--     }
--   (do
--     Lucid.tr_ $ do
--       Lucid.td_ [Lucid.class_ "px-4 py-3"] "John"
--       Lucid.td_ [Lucid.class_ "px-4 py-3 text-right"] "30"
--       Lucid.td_ [Lucid.class_ "px-4 py-3"] "john\@example.com"
--   )
-- @
renderTable ::
  -- | Table configuration
  TableConfig ->
  -- | Table body content (rows)
  Lucid.Html () ->
  Lucid.Html ()
renderTable config bodyContent =
  Lucid.div_ [Lucid.class_ config.wrapperClass] $
    Lucid.table_ [Lucid.class_ config.tableClass] $ do
      Lucid.thead_ [class_ $ base [Tokens.bgGray800, Tokens.textWhite]] $
        Lucid.tr_ $
          mapM_ renderHeader config.headers
      Lucid.tbody_ bodyContent
  where
    renderHeader :: ColumnHeader -> Lucid.Html ()
    renderHeader colHeader =
      Lucid.th_ [class_ $ base [Tokens.p4, alignClass colHeader.headerAlign]] $
        Lucid.toHtml colHeader.headerText

    alignClass :: ColumnAlign -> Text
    alignClass AlignLeft = "text-left"
    alignClass AlignRight = "text-right"
    alignClass AlignCenter = "text-center"
