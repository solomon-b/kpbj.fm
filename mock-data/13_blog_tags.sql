-- Blog Tags
-- Categories for blog posts

INSERT INTO blog_tags (name) VALUES
  ('award'),
  ('community'),
  ('underground'),
  ('interviews'),
  ('local-scene'),
  ('fundraising'),
  ('new-shows'),
  ('music-discovery')
ON CONFLICT (name) DO NOTHING;
