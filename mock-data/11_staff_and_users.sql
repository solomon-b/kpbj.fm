-- Staff and Regular Users
-- Password for all users: "password"

-- Staff user
INSERT INTO users (email, password) VALUES
  ('staff@kpbj.fm', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I')
ON CONFLICT (email) DO NOTHING;

INSERT INTO user_metadata (user_id, display_name, full_name, avatar_url, user_role)
SELECT u.id, 'Staff Member', 'KPBJ Staff', 'images/2025/01/01/avatars/staff.jpg', 'Staff'
FROM users u WHERE u.email = 'staff@kpbj.fm';

-- Regular listener accounts
INSERT INTO users (email, password) VALUES
  ('listener@example.com', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I'),
  ('musicfan@example.com', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I'),
  ('vinyl.collector@example.com', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I'),
  ('portland.local@example.com', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I'),
  ('nightowl@example.com', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I')
ON CONFLICT (email) DO NOTHING;

INSERT INTO user_metadata (user_id, display_name, full_name, avatar_url, user_role)
SELECT u.id, 'RadioHead42', 'Alex Thompson', 'images/2025/01/01/avatars/radiovolunteer.jpg', 'User'
FROM users u WHERE u.email = 'listener@example.com';

INSERT INTO user_metadata (user_id, display_name, full_name, user_role)
SELECT u.id, 'JazzCat', 'Maria Santos', 'User'
FROM users u WHERE u.email = 'musicfan@example.com';

INSERT INTO user_metadata (user_id, display_name, full_name, avatar_url, user_role)
SELECT u.id, 'VinylJunkie', 'Sam Chen', 'images/2025/01/01/avatars/vinylhead.jpg', 'User'
FROM users u WHERE u.email = 'vinyl.collector@example.com';

INSERT INTO user_metadata (user_id, display_name, full_name, user_role)
SELECT u.id, 'PDXBeats', 'Jordan Rivera', 'User'
FROM users u WHERE u.email = 'portland.local@example.com';

INSERT INTO user_metadata (user_id, display_name, full_name, avatar_url, user_role)
SELECT u.id, 'MidnightListener', 'Casey Park', 'images/2025/01/01/avatars/midnightlistener.jpg', 'User'
FROM users u WHERE u.email = 'nightowl@example.com';
