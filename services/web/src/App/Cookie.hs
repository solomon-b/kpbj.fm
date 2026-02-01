-- | Cookie migration helpers.
--
-- Session cookie creation is now handled by 'App.Auth.mkCookieSession' from
-- @web-server-core@. This module only contains migration code for expiring
-- old cookie formats.
--
-- __TODO (after 2026-05-01):__ Remove 'mkExpireOldSessionCookie' and related
-- migration code. See the function's documentation for details.
module App.Cookie
  ( mkExpireOldSessionCookie,
  )
where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import Data.Foldable (fold)
import Data.Text (Text)

--------------------------------------------------------------------------------

-- | Create a Set-Cookie header to expire the old session cookie (without Domain).
--
-- == Why This Exists
--
-- Before 2026-01-28, session cookies were set without a Domain attribute.
-- When we added cross-subdomain authentication (for uploads.kpbj.fm), we needed
-- to add @Domain=.kpbj.fm@ to cookies. However, browsers treat cookies with and
-- without Domain as separate cookies, so users would have TWO session cookies:
--
-- 1. Old cookie: @session-id=xyz@ (no Domain, only sent to www.kpbj.fm)
-- 2. New cookie: @session-id=xyz; Domain=.kpbj.fm@ (sent to all subdomains)
--
-- This function creates a Set-Cookie header that expires the old cookie format,
-- ensuring users only have the new domain-scoped cookie.
--
-- == When To Remove This
--
-- This migration code can be safely removed after 2026-05-01 (approximately
-- 3 months after deployment). By then:
--
-- - All active users will have logged in at least once with the new cookie format
-- - Old session cookies will have naturally expired or been replaced
-- - Inactive users will simply need to log in again (normal behavior)
--
-- == How To Remove
--
-- 1. Delete this function ('mkExpireOldSessionCookie')
-- 2. Remove the second @Set-Cookie@ header from login/verify-email handlers:
--    - @src/API/User/Login/Post/Handler.hs@
--    - @src/API/User/Login/Post/Route.hs@
--    - @src/API/User/VerifyEmail/Get/Handler.hs@
--    - @src/API/User/VerifyEmail/Get/Route.hs@
-- 3. Update tests in @test/App/CookieSpec.hs@ (remove expiration cookie tests)
-- 4. Simplify the route types back to single @Set-Cookie@ header
--
-- Returns Nothing for Development (no migration needed).
mkExpireOldSessionCookie :: Environment -> Maybe Text
mkExpireOldSessionCookie = \case
  Development -> Nothing
  _ ->
    Just $
      fold
        [ "session-id",
          "=",
          "; ",
          "Path=/",
          "; ",
          "HttpOnly",
          "; ",
          "Secure",
          "; ",
          "Max-Age=0"
        ]
