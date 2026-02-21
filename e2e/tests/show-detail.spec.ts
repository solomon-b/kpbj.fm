import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Show detail page (/shows/:slug)
// ---------------------------------------------------------------------------
// Tests use "Sunday Morning Jazz" as the fixture show because it has known
// mock data: host (Smooth Sullivan), tags (Jazz, Soul), description,
// logo image, schedule, and 3 episodes.

const SHOW_SLUG = "sunday-morning-jazz";
const SHOW_TITLE = "SUNDAY MORNING JAZZ"; // rendered uppercase
const SHOW_URL = `/shows/${SHOW_SLUG}`;

test.describe("Show detail", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto(SHOW_URL);
  });

  // -------------------------------------------------------------------------
  // Show header
  // -------------------------------------------------------------------------

  test("displays the show title in uppercase", async ({ page }) => {
    await expect(page.locator("h1")).toContainText(SHOW_TITLE);
  });

  test("displays the show description", async ({ page }) => {
    await expect(
      page.getByText("Start your Sunday with smooth jazz classics")
    ).toBeVisible();
  });

  test("displays the show logo with alt text", async ({ page }) => {
    const logo = page.locator('img[alt="Sunday Morning Jazz logo"]');
    await expect(logo).toBeVisible();
  });

  test("displays the host name", async ({ page }) => {
    await expect(page.getByText("Smooth Sullivan")).toBeVisible();
  });

  test("displays the schedule", async ({ page }) => {
    // Schedule renders as "Sundays <time range>" (no "Every" prefix when
    // the show airs all 5 weeks). Sunday Morning Jazz airs 12am-2am.
    await expect(page.getByText(/Sundays/)).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Tags
  // -------------------------------------------------------------------------

  test("displays show tags", async ({ page }) => {
    await expect(page.getByText("#Jazz")).toBeVisible();
    await expect(page.getByText("#Soul")).toBeVisible();
  });

  test("clicking a tag navigates to shows filtered by that tag", async ({ page }) => {
    await page.getByText("#Jazz").click();

    // Should navigate to the shows index with a tag filter param.
    await expect(page).toHaveURL(/\/shows\?.*tag=/);
  });

  test("tag filter shows matching shows on the shows index", async ({ page }) => {
    // Click #Jazz tag — mock data has 5 shows tagged "Jazz".
    await page.getByText("#Jazz").click();
    await page.waitForURL(/\/shows\?.*tag=/);

    // The shows index should display only Jazz-tagged shows.
    const cards = page.locator("#shows-list > a");
    await expect(cards).toHaveCount(5);
  });

  // -------------------------------------------------------------------------
  // Episodes
  // -------------------------------------------------------------------------

  test("displays episode cards", async ({ page }) => {
    // Sunday Morning Jazz has 3 episodes in mock data.
    // Scope to #main-content to avoid matching the navbar player.
    const episodes = page.locator("#main-content [x-data*=playerId]");
    await expect(episodes).toHaveCount(3);
  });

  test("episode cards show dates", async ({ page }) => {
    // Episodes have dates in "Month DD, YYYY" format.
    const firstEpisode = page.locator("#main-content [x-data*=playerId]").first();
    await expect(firstEpisode.getByText(/\w+ \d{1,2}, \d{4}/)).toBeVisible();
  });

  test("episode cards have artwork or placeholder", async ({ page }) => {
    const firstEpisode = page.locator("#main-content [x-data*=playerId]").first();
    // Should have either an img or the "[EP IMG]" placeholder.
    const hasImage = (await firstEpisode.locator("img").count()) > 0;
    const hasPlaceholder = (await firstEpisode.getByText("[EP IMG]").count()) > 0;
    expect(hasImage || hasPlaceholder).toBe(true);
  });

  test("clicking episode artwork navigates to episode detail", async ({ page }) => {
    // The artwork is wrapped in an <a> with an href to the episode page.
    const firstEpisode = page.locator("#main-content [x-data*=playerId]").first();
    const artworkLink = firstEpisode.locator("a").first();
    await artworkLink.click();

    await expect(page).toHaveURL(/\/shows\/sunday-morning-jazz\/episodes\//);
  });

  test("episode cards have a play button", async ({ page }) => {
    const firstEpisode = page.locator("#main-content [x-data*=playerId]").first();
    // The circular play button overlay on the artwork.
    const playButton = firstEpisode.locator("button[x-on\\:click='toggle()']");
    await expect(playButton).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Pagination
  // -------------------------------------------------------------------------
  // Sunday Morning Jazz has 3 episodes, and page size is 10, so pagination
  // should NOT appear (no next page needed).

  test("does not show pagination when all episodes fit on one page", async ({ page }) => {
    // With only 3 episodes (<10), neither Previous nor Next should be active links.
    await expect(page.locator("a", { hasText: "Next" })).not.toBeAttached();
  });
});
