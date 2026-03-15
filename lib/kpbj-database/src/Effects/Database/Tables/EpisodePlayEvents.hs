{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @episode_play_events@.
--
-- Stores a log of when users play archived episodes via the browser audio
-- player. Populated by POST /api/analytics/episode-play.
module Effects.Database.Tables.EpisodePlayEvents
  ( -- * Queries
    insertPlayEvent,
    getDailyPlayCounts,
    getTopEpisodes,
    getTotalPlays,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Effects.Database.Tables.Episodes qualified as Episodes
import GHC.Generics (Generic)
import Hasql.Interpolate (OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Rel8

--------------------------------------------------------------------------------
-- Table Definition

-- | The @episode_play_events@ table definition using rel8's higher-kinded data pattern.
data EpisodePlayEvent f = EpisodePlayEvent
  { epeId :: Column f Int64,
    epeEpisodeId :: Column f Int64,
    epePlayedAt :: Column f UTCTime
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
            epeEpisodeId = "episode_id",
            epePlayedAt = "played_at"
          }
    }

--------------------------------------------------------------------------------
-- Queries

-- | Get daily play counts within a time range.
--
-- Returns (count, date_label) where date_label is formatted as @YYYY-MM-DD@.
getDailyPlayCounts :: UTCTime -> UTCTime -> Hasql.Statement () [(Int64, Text)]
getDailyPlayCounts start end = interp False
  [sql|
    SELECT COUNT(*)::bigint, to_char(date_trunc('day', played_at), 'YYYY-MM-DD') AS day_label
    FROM episode_play_events
    WHERE played_at >= #{start} AND played_at < #{end}
    GROUP BY day_label
    ORDER BY day_label
  |]


-- | Get top N episodes by play count within a time range.
--
-- Returns: (episode_id, show_title, episode_number, play_count)
getTopEpisodes :: UTCTime -> UTCTime -> Int32 -> Hasql.Statement () [(Int64, Text, Int64, Int64)]
getTopEpisodes start end n = interp False
  [sql|
    SELECT e.id, s.title, e.episode_number::bigint, COUNT(*)::bigint
    FROM episode_play_events epe
    JOIN episodes e ON e.id = epe.episode_id
    JOIN shows s ON s.id = e.show_id
    WHERE epe.played_at >= #{start} AND epe.played_at < #{end}
    GROUP BY e.id, s.title, e.episode_number
    ORDER BY COUNT(*) DESC
    LIMIT #{n}
  |]


-- | Get total play count within a time range.
--
-- Returns 'Nothing' if there are no play events in the range.
getTotalPlays :: UTCTime -> UTCTime -> Hasql.Statement () (Maybe Int64)
getTotalPlays start end =
  fmap getOneColumn
    <$> interp
      False
      [sql|
        SELECT NULLIF(COUNT(*), 0)::bigint
        FROM episode_play_events
        WHERE played_at >= #{start} AND played_at < #{end}
      |]


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
                    epeEpisodeId = lit episodeId,
                    epePlayedAt = unsafeDefault
                  }
              ],
          onConflict = Abort,
          returning = NoReturning
        }
