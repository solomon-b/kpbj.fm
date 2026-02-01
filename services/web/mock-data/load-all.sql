-- Mock Data Loader
-- Executes all mock data SQL files in order
-- Usage: psql -f mock-data/load-all.sql

\echo '=========================================='
\echo 'KPBJ Mock Data Loader'
\echo '=========================================='

\echo ''
\echo '00. Initializing (truncating existing data)...'
\i 00_init.sql

\echo ''
\echo '01. Creating admin user...'
\i 01_admin_user.sql

\echo ''
\echo '02. Creating shows...'
\i 02_shows.sql

\echo ''
\echo '03. Creating show tags...'
\i 03_show_tags.sql

\echo ''
\echo '04. Assigning tags to shows...'
\i 04_show_tag_assignments.sql

\echo ''
\echo '05. Creating host users...'
\i 05_host_users.sql

\echo ''
\echo '06. Creating host user metadata...'
\i 06_host_user_metadata.sql

\echo ''
\echo '07. Linking hosts to shows...'
\i 07_show_hosts.sql

\echo ''
\echo '08. Creating host details...'
\i 08_host_details.sql

\echo ''
\echo '09. Creating schedule templates...'
\i 09_schedule_templates.sql

\echo ''
\echo '10. Generating episodes...'
\i 10_episodes.sql

\echo ''
\echo '11. Creating staff and regular users...'
\i 11_staff_and_users.sql

\echo ''
\echo '12. Creating events...'
\i 12_events.sql

\echo ''
\echo '13. Creating blog tags...'
\i 13_blog_tags.sql

\echo ''
\echo '14. Creating blog posts...'
\i 14_blog_posts.sql

\echo ''
\echo '15. Generating episode tracks...'
\i 15_episode_tracks.sql

\echo ''
\echo '17. Creating episode tags...'
\i 17_episode_tags.sql

\echo ''
\echo '18. Assigning tags to episodes...'
\i 18_episode_tag_assignments.sql

\echo ''
\echo '19. Verifying all user accounts...'
\i 19_verify_users.sql

\echo ''
\echo '=========================================='
\echo 'Summary'
\echo '=========================================='
\i 16_summary.sql

\echo ''
\echo 'Mock data loaded successfully!'
