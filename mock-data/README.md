# KPBJ Mock Data

This directory contains SQL scripts to populate the development database with realistic test data.

## Files

The SQL files are numbered to indicate execution order:

| File | Description |
|------|-------------|
| `00_init.sql` | Truncates existing data and prepares for fresh load |
| `01_admin_user.sql` | Admin user account |
| `02_shows.sql` | 84 radio shows (24/7 schedule coverage) |
| `03_show_tags.sql` | Genre tags for categorizing shows |
| `04_show_tag_assignments.sql` | Links shows to their genre tags |
| `05_host_users.sql` | User accounts for each show host |
| `06_host_user_metadata.sql` | Display names and full names for hosts |
| `07_show_hosts.sql` | Links hosts to their shows |
| `08_host_details.sql` | Host bios and social media links |
| `09_schedule_templates.sql` | Weekly schedule for each show |
| `10_episodes.sql` | Generated episodes for past 2 weeks |
| `11_staff_and_users.sql` | Staff and regular listener accounts |
| `12_events.sql` | Community events |
| `13_blog_tags.sql` | Tags for blog posts |
| `14_blog_posts.sql` | Station blog content |
| `15_episode_tracks.sql` | Track library and episode playlists |
| `16_summary.sql` | Summary statistics query |

## Usage

Load all mock data into the development database:

```bash
just mock-data
```

This will execute all SQL files in order against the `dev_db` database.

## Test Accounts

All user accounts use the password: `password`

| Email | Role | Description |
|-------|------|-------------|
| `admin@kpbj.fm` | Admin | Site administrator |
| `staff@kpbj.fm` | Staff | Station staff member |
| `host-{show-slug}@kpbj.fm` | Host | Show host (84 accounts) |
| `listener@example.com` | User | Test listener account |
| `musicfan@example.com` | User | Test listener account |
| `vinyl.collector@example.com` | User | Test listener account |
| `portland.local@example.com` | User | Test listener account |
| `nightowl@example.com` | User | Test listener account |

## Data Overview

- **84 Shows**: Full 24/7 schedule with diverse genres
- **84 Host Users**: One host per show with themed display names
- **168 Episodes**: 2 weeks of episodes (2 per show)
- **1,680 Episode Tracks**: 10 tracks per episode
- **12 Events**: Community events over several months
- **8 Blog Posts**: Station news and content
- **68 Genre Tags**: For categorizing shows

## Media Files

The `media/` subdirectory contains placeholder media files for development.
