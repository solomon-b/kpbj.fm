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
  ( -- * Link generation structures
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
    dashboardShowsLinks,
    dashboardEventsLinks,
    dashboardUsersLinks,
  )
where

--------------------------------------------------------------------------------

import API.Types
import Domain.Types.WeekOffset (WeekOffset)
import Servant.Links (AsLink, Link, allFieldLinks)

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

-- | Dashboard shows route links
dashboardShowsLinks :: DashboardShowsRoutes (AsLink Link)
dashboardShowsLinks = dashboardAdminLinks.shows

-- | Dashboard events route links
dashboardEventsLinks :: DashboardEventsRoutes (AsLink Link)
dashboardEventsLinks = dashboardAdminLinks.events

-- | Dashboard users route links
dashboardUsersLinks :: DashboardUsersRoutes (AsLink Link)
dashboardUsersLinks = dashboardAdminLinks.users
