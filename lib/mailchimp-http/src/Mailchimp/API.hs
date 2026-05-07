-- | Servant API type definitions for the Mailchimp Marketing API v3.
--
-- Defines the subset of Mailchimp endpoints used by this application. These
-- types are consumed by "Mailchimp.Client" to derive request functions via
-- @servant-client@.
module Mailchimp.API
  ( -- * Individual Endpoints
    UpsertMemberAPI,
    ArchiveMemberAPI,
    ListMembersAPI,

    -- * Combined API
    MailchimpAPI,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Mailchimp.Types
  ( ListMembersResponse,
    Member,
    UpsertMemberBody,
  )
import Servant.API
  ( Capture,
    Delete,
    Get,
    Header',
    JSON,
    NoContent,
    Put,
    QueryParam,
    ReqBody,
    Required,
    Strict,
    (:<|>),
    (:>),
  )

--------------------------------------------------------------------------------

-- | @PUT /3.0/lists/{list_id}/members/{subscriber_hash}@
--
-- Creates or updates an audience member. The body uses @status_if_new@ so
-- that resubmitting a signup for an already-unsubscribed address does not
-- silently resubscribe them.
type UpsertMemberAPI =
  "lists"
    :> Capture "listId" Text
    :> "members"
    :> Capture "subscriberHash" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> ReqBody '[JSON] UpsertMemberBody
    :> Put '[JSON] Member

-- | @DELETE /3.0/lists/{list_id}/members/{subscriber_hash}@
--
-- Archives (soft-deletes) the member. History is preserved so an admin can
-- re-add the address later.
type ArchiveMemberAPI =
  "lists"
    :> Capture "listId" Text
    :> "members"
    :> Capture "subscriberHash" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Delete '[JSON] NoContent

-- | @GET /3.0/lists/{list_id}/members@
--
-- Lists audience members. The @count@/@offset@ parameters drive
-- pagination; the @fields@ parameter trims the response to just the
-- attributes the reconcile job needs.
type ListMembersAPI =
  "lists"
    :> Capture "listId" Text
    :> "members"
    :> QueryParam "count" Int
    :> QueryParam "offset" Int
    :> QueryParam "fields" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Get '[JSON] ListMembersResponse

-- | Combined Mailchimp API used to derive client functions.
type MailchimpAPI =
  UpsertMemberAPI
    :<|> ArchiveMemberAPI
    :<|> ListMembersAPI
