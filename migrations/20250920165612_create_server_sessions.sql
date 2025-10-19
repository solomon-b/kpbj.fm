CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE server_sessions
  ( id UUID PRIMARY KEY NOT NULL DEFAULT uuid_generate_v1mc ()
  , user_id SERIAL8 NOT NULL REFERENCES users (id) ON DELETE CASCADE
  , ip_address inet
  , user_agent text
  , expires_at timestamptz NOT NULL
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
  , updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

CREATE OR REPLACE FUNCTION check_only_one_active_session()
  RETURNS TRIGGER AS $$
  BEGIN
    IF (SELECT COUNT(*)
        FROM server_sessions s
        WHERE s.user_id = NEW.user_id
          AND s.expires_at > now()
       ) <= 1
    THEN
      RETURN NEW;
    ELSE
      RAISE EXCEPTION 'Cannot insert multiple active sessions for user %', NEW.user_id;
    END IF;
  END
  $$ LANGUAGE 'plpgsql';

CREATE CONSTRAINT TRIGGER check_only_one_active_session
  AFTER INSERT OR UPDATE
  ON server_sessions
  FOR EACH ROW
  EXECUTE PROCEDURE check_only_one_active_session();
