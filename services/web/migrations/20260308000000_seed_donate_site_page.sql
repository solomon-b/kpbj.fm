INSERT INTO site_pages (slug, title, content, updated_at)
VALUES (
  'donate',
  'Support KPBJ',
  E'KPBJ 95.9FM is a volunteer-run, independent community radio station. We rely on listener support to keep broadcasting.\n\nYour donation helps cover:\n\n- **Equipment maintenance** and upgrades\n- **Streaming costs** to reach listeners online\n- **Community events** and programming\n\nEvery contribution, no matter the size, makes a difference. Thank you for supporting independent radio.',
  NOW()
)
ON CONFLICT (slug) DO NOTHING;
