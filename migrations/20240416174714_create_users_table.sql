CREATE TABLE users
  ( id SERIAL8 PRIMARY KEY UNIQUE
  , email VARCHAR NOT NULL UNIQUE
  , password VARCHAR NOT NULL
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
  , updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
  , deleted_at TIMESTAMP WITH TIME ZONE DEFAULT NULL
  );

-- Create index for filtering out deleted users (partial index for active users only)
CREATE INDEX idx_users_deleted_at ON users(deleted_at) WHERE deleted_at IS NULL;

-- Add comment for documentation
COMMENT ON COLUMN users.deleted_at IS 'Soft delete timestamp - NULL means active user, non-NULL means deleted. Deleted users cannot authenticate.';
