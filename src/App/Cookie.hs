-- | Cookie helpers with domain support for cross-subdomain authentication.
--
-- This module provides cookie creation functions that include the Domain attribute,
-- allowing cookies to be shared across subdomains (e.g., staging.kpbj.fm and
-- uploads.staging.kpbj.fm).
--
-- Domain configuration is centralized in "App.Domains".
--
-- __TODO (after 2026-05-01):__ Remove 'mkExpireOldSessionCookie' and related
-- migration code. See the function's documentation for details.
module App.Cookie
  ( mkCookieSessionWithDomain,
    mkExpireOldSessionCookie,
    lookupSessionId,
  )
where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import App.Domains qualified as Domains
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text.Encoding
import Data.UUID qualified as UUID
import Effects.Database.Tables.ServerSessions qualified as ServerSessions
import Web.Cookie (parseCookies)

--------------------------------------------------------------------------------

-- | Create a session cookie with the appropriate Domain attribute.
--
-- The Domain attribute allows the cookie to be shared across subdomains.
-- See "App.Domains" for the domain and cookie name configuration per environment.
mkCookieSessionWithDomain :: Environment -> ServerSessions.Id -> Text
mkCookieSessionWithDomain env sId =
  let baseCookie =
        fold
          [ Domains.cookieName env,
            "=",
            display sId,
            "; ",
            "SameSite=lax",
            "; ",
            "Path=/",
            "; ",
            "HttpOnly",
            "; ",
            "Secure"
          ]
      domainAttr = Domains.cookieDomain env
   in case domainAttr of
        "" -> baseCookie
        domain -> baseCookie <> "; Domain=" <> domain

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

--------------------------------------------------------------------------------

-- | Parse a session ID from a raw cookie header using the environment-specific cookie name.
--
-- This looks up the correct cookie name for the environment (e.g., "session-id-production"
-- for production, "session-id-staging" for staging) and parses the UUID value.
lookupSessionId :: Environment -> Text -> Maybe ServerSessions.Id
lookupSessionId env cookieHeader =
  let cookieBytes = Text.Encoding.encodeUtf8 cookieHeader
      nameBytes = Text.Encoding.encodeUtf8 (Domains.cookieName env)
   in lookup nameBytes (parseCookies cookieBytes)
        >>= UUID.fromASCIIBytes
        >>= Just . ServerSessions.Id
