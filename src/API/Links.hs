{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Type-safe link generation for all API routes.
--
-- This module provides link records that can be imported and used with
-- record dot syntax. It only depends on API.Types (route type definitions),
-- not on handler modules.
--
-- Usage:
--
-- @
-- import API.Links (apiLinks, blogLinks)
-- import Servant.Links qualified as Links
--
-- blogPostUrl postId slug = Links.linkURI $ blogLinks.postWithSlug postId slug
-- aboutUrl = Links.linkURI apiLinks.aboutGet
-- @
module API.Links
  ( -- * Link helpers
    rootLink,

    -- * Link generation structures
    apiLinks,
    blogLinks,
    eventsLinks,
    scheduleLink,
    showsLinks,
    showBlogLinks,
    showEpisodesLinks,
    userLinks,
    dashboardLinks,
    dashboardHostLinks,
    dashboardAdminLinks,
    dashboardEpisodesLinks,
    dashboardBlogsLinks,
    dashboardStationBlogLinks,
    dashboardStationIdsLinks,
    dashboardEphemeralUploadsLinks,
    dashboardShowsLinks,
    dashboardEventsLinks,
    dashboardUsersLinks,
    dashboardSitePagesLinks,
  )
where

--------------------------------------------------------------------------------

import API.Types
import Data.Text (Text)
import Domain.Types.WeekOffset (WeekOffset)
import Servant.Links (AsLink, Link, allFieldLinks)
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

-- | Convert a Servant Link to a root-relative URL path for HTML.
--
-- Servant's 'Link' produces relative paths (e.g., @"user/login"@).
-- For HTML href attributes, we need root-relative paths (e.g., @"/user/login"@)
-- to ensure links resolve correctly regardless of the current page's URL.
--
-- Usage:
--
-- @
-- Lucid.a_ [Lucid.href_ (rootLink $ userLinks.loginGet Nothing Nothing)] "Login"
-- @
rootLink :: Link -> Text
rootLink = ("/" <>) . Http.toUrlPiece

--------------------------------------------------------------------------------

-- | All API links generated from NamedRoutes structure
apiLinks :: Routes (AsLink Link)
apiLinks = allFieldLinks

-- | Blog route links
blogLinks :: BlogRoutes (AsLink Link)
blogLinks = apiLinks.blog

-- | Events route links
eventsLinks :: EventsRoutes (AsLink Link)
eventsLinks = apiLinks.events

-- | Schedule route link (top-level /schedule)
scheduleLink :: Maybe WeekOffset -> Link
scheduleLink = apiLinks.schedule

-- | Shows route links
showsLinks :: ShowsRoutes (AsLink Link)
showsLinks = apiLinks.shows

-- | Show blog route links
showBlogLinks :: ShowBlogRoutes (AsLink Link)
showBlogLinks = showsLinks.blog

-- | Show episodes route links
showEpisodesLinks :: ShowEpisodesRoutes (AsLink Link)
showEpisodesLinks = showsLinks.episodes

-- | User auth route links
userLinks :: UserRoutes (AsLink Link)
userLinks = user apiLinks

-- | Dashboard route links
dashboardLinks :: DashboardRoutes (AsLink Link)
dashboardLinks = apiLinks.dashboard

-- | Dashboard host route links
dashboardHostLinks :: DashboardHostRoutes (AsLink Link)
dashboardHostLinks = dashboardLinks.host

-- | Dashboard admin route links
dashboardAdminLinks :: DashboardAdminRoutes (AsLink Link)
dashboardAdminLinks = dashboardLinks.admin

-- | Dashboard episodes route links
dashboardEpisodesLinks :: DashboardEpisodesRoutes (AsLink Link)
dashboardEpisodesLinks = dashboardHostLinks.episodes

-- | Dashboard blogs route links
dashboardBlogsLinks :: DashboardBlogsRoutes (AsLink Link)
dashboardBlogsLinks = blogs dashboardHostLinks

-- | Dashboard station blog route links
dashboardStationBlogLinks :: DashboardStationBlogRoutes (AsLink Link)
dashboardStationBlogLinks = stationBlog dashboardAdminLinks

-- | Dashboard station IDs route links
dashboardStationIdsLinks :: DashboardStationIdsRoutes (AsLink Link)
dashboardStationIdsLinks = dashboardHostLinks.stationIds

-- | Dashboard ephemeral uploads route links
dashboardEphemeralUploadsLinks :: DashboardEphemeralUploadsRoutes (AsLink Link)
dashboardEphemeralUploadsLinks = dashboardHostLinks.ephemeralUploads

-- | Dashboard shows route links
dashboardShowsLinks :: DashboardShowsRoutes (AsLink Link)
dashboardShowsLinks = dashboardAdminLinks.shows

-- | Dashboard events route links
dashboardEventsLinks :: DashboardEventsRoutes (AsLink Link)
dashboardEventsLinks = dashboardAdminLinks.events

-- | Dashboard users route links
dashboardUsersLinks :: DashboardUsersRoutes (AsLink Link)
dashboardUsersLinks = dashboardAdminLinks.users

-- | Dashboard site pages route links
dashboardSitePagesLinks :: DashboardSitePagesRoutes (AsLink Link)
dashboardSitePagesLinks = dashboardAdminLinks.sitePages
