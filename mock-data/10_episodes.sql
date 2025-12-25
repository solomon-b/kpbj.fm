-- Episodes
-- Generated episodes for the past 3 weeks based on show schedules
-- Each show gets 3 episodes (one per week for 3 weeks)

WITH episode_data AS (
    SELECT
        s.id as show_id,
        s.slug as show_slug,
        st.id as schedule_template_id,
        'A great episode of ' || s.title || ' from ' || to_char(generate_series, 'FMMonth DD, YYYY') as description,
        'published' as status,
        -- interpret date + time in show's timezone, then convert to UTC
        (generate_series::date::text || ' ' || st.start_time)::timestamp AT TIME ZONE st.timezone as scheduled_at,
        (generate_series::date::text || ' ' || st.start_time)::timestamp AT TIME ZONE st.timezone as published_at,
        u.id as created_by,
        ROW_NUMBER() OVER (PARTITION BY s.id ORDER BY generate_series DESC) as episode_num
    FROM shows s
    JOIN schedule_templates st ON st.show_id = s.id
    JOIN schedule_template_validity stv ON stv.template_id = st.id
    JOIN users u ON u.email = 'host-' || s.slug || '@kpbj.fm'
    CROSS JOIN LATERAL (
        -- Generate dates for the past 3 weeks for this show's day of week
        SELECT generate_series
        FROM generate_series(
            CURRENT_DATE - INTERVAL '21 days',
            CURRENT_DATE - INTERVAL '1 day',
            INTERVAL '1 day'
        ) AS generate_series
        WHERE EXTRACT(DOW FROM generate_series)::INTEGER =
            CASE st.day_of_week::TEXT
                WHEN 'sunday' THEN 0
                WHEN 'monday' THEN 1
                WHEN 'tuesday' THEN 2
                WHEN 'wednesday' THEN 3
                WHEN 'thursday' THEN 4
                WHEN 'friday' THEN 5
                WHEN 'saturday' THEN 6
            END
    ) dates
    WHERE stv.effective_from <= CURRENT_DATE - INTERVAL '21 days'
      AND (stv.effective_until IS NULL OR stv.effective_until > CURRENT_DATE - INTERVAL '1 day')
)
INSERT INTO episodes (show_id, schedule_template_id, description, artwork_url, status, scheduled_at, published_at, created_by)
SELECT
    show_id,
    schedule_template_id,
    description,
    'images/2025/01/01/artwork/' || show_slug || '-ep' || episode_num || '.jpg' as artwork_url,
    status,
    scheduled_at,
    published_at,
    created_by
FROM episode_data
WHERE episode_num <= 3  -- Only include up to 3 episodes per show (matching available artwork)
ORDER BY show_id, scheduled_at DESC;
