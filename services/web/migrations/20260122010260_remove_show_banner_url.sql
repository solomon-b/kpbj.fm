-- Remove banner_url column from shows table
-- Banner/wallpaper images are no longer supported

ALTER TABLE shows DROP COLUMN IF EXISTS banner_url;
