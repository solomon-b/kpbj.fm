-- Create suspension_status domain with enum values
CREATE DOMAIN suspension_status AS TEXT CHECK (VALUE IN ('NotSuspended', 'Suspended'));

CREATE TABLE users
  ( id SERIAL8 PRIMARY KEY UNIQUE
  , email VARCHAR NOT NULL UNIQUE
  , password VARCHAR NOT NULL
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
  , updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
  , deleted_at TIMESTAMP WITH TIME ZONE DEFAULT NULL
  , suspended_at TIMESTAMP WITH TIME ZONE DEFAULT NULL
  , suspension_status suspension_status NOT NULL DEFAULT 'NotSuspended'
  , suspension_reason TEXT DEFAULT NULL
  );

-- Create index for filtering out deleted users (partial index for active users only)
CREATE INDEX idx_users_deleted_at ON users(deleted_at) WHERE deleted_at IS NULL;

-- Create partial index for filtering suspended users (optimizes queries for active users)
CREATE INDEX idx_users_suspended_at ON users(suspended_at) WHERE suspended_at IS NULL;

-- Add comments for documentation
COMMENT ON COLUMN users.deleted_at IS 'Soft delete timestamp - NULL means active user, non-NULL means deleted. Deleted users cannot authenticate.';
COMMENT ON COLUMN users.suspended_at IS 'User suspension timestamp - NULL means active, non-NULL means suspended. Suspended users cannot perform host actions but can still log in to see the suspension notice.';
COMMENT ON COLUMN users.suspension_status IS 'Current suspension status - NotSuspended or Suspended. Used for quick status checks without timestamp comparison.';
COMMENT ON COLUMN users.suspension_reason IS 'Admin-provided reason for suspension (for audit trail and user communication).';
