{-# LANGUAGE BlockArguments #-}

-- | Database table definition and queries for @episode_play_events@.
--
-- Stores a log of when users play archived episodes via the browser audio
-- player. Populated by POST /api/analytics/episode-play.
module Effects.Database.Tables.EpisodePlayEvents
  ( -- * Queries
    insertPlayEvent,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Effects.Database.Tables.Episodes qualified as Episodes
import GHC.Generics (Generic)
import Hasql.Statement qualified as Hasql
import Rel8

--------------------------------------------------------------------------------
-- Table Definition

-- | The @episode_play_events@ table definition using rel8's higher-kinded data pattern.
data EpisodePlayEvent f = EpisodePlayEvent
  { epeId :: Column f Int64,
    epeEpisodeId :: Column f Int64
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

episodePlayEventsSchema :: TableSchema (EpisodePlayEvent Name)
episodePlayEventsSchema =
  TableSchema
    { name = "episode_play_events",
      columns =
        EpisodePlayEvent
          { epeId = "id",
            epeEpisodeId = "episode_id"
          }
    }

--------------------------------------------------------------------------------
-- Queries

-- | Insert a new episode play event.
insertPlayEvent :: Episodes.Id -> Hasql.Statement () ()
insertPlayEvent (Episodes.Id episodeId) =
  run_ $
    insert
      Insert
        { into = episodePlayEventsSchema,
          rows =
            values
              [ EpisodePlayEvent
                  { epeId = unsafeDefault,
                    epeEpisodeId = lit episodeId
                  }
              ],
          onConflict = Abort,
          returning = NoReturning
        }
