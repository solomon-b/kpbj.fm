import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Episode detail page (/shows/:slug/episodes/:number)
// ---------------------------------------------------------------------------
// Tests use Sunday Morning Jazz episode 1 (most recent). Mock data gives
// each episode: a description, 10 tracks (jazz genre), and the "classics"
// episode tag. Episodes are numbered newest-first (1 = most recent).

const EPISODE_URL = "/shows/sunday-morning-jazz/episodes/1";

test.describe("Episode detail", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto(EPISODE_URL);
  });

  // -------------------------------------------------------------------------
  // Episode card
  // -------------------------------------------------------------------------

  test("displays episode artwork or placeholder", async ({ page }) => {
    const card = page.locator("#main-content [x-data*=playerId]").first();
    const hasImage = (await card.locator("img").count()) > 0;
    const hasPlaceholder = (await card.getByText("[EP IMG]").count()) > 0;
    expect(hasImage || hasPlaceholder).toBe(true);
  });

  test("displays episode date", async ({ page }) => {
    // Date is in a small muted div, distinct from the description paragraph.
    // Target the specific text-sm element that contains the date.
    const dateEl = page.locator("#main-content [x-data*=playerId] .text-sm");
    await expect(dateEl).toBeVisible();
    const dateText = await dateEl.textContent();
    expect(dateText).toMatch(/\w+ \d{1,2}, \d{4}/);
  });

  test("has a play button", async ({ page }) => {
    const card = page.locator("#main-content [x-data*=playerId]").first();
    const playButton = card.locator("button[x-on\\:click='toggle()']");
    await expect(playButton).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Description
  // -------------------------------------------------------------------------

  test("displays the episode description", async ({ page }) => {
    // Mock data generates descriptions like:
    // "A great episode of Sunday Morning Jazz from <date>"
    await expect(
      page.getByText(/A great episode of Sunday Morning Jazz/)
    ).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Tags
  // -------------------------------------------------------------------------

  test("displays episode tags", async ({ page }) => {
    // Sunday Morning Jazz episodes always get the "classics" tag.
    await expect(page.getByText("#classics")).toBeVisible();
  });

  test("clicking a tag navigates to the shows index", async ({ page }) => {
    await page.getByText("#classics").click();
    await expect(page).toHaveURL(/\/shows/);
  });

  // -------------------------------------------------------------------------
  // Track listing
  // -------------------------------------------------------------------------

  test("displays the track listing heading", async ({ page }) => {
    await expect(
      page.locator("h2", { hasText: "Track Listing" })
    ).toBeVisible();
  });

  test("displays 10 tracks", async ({ page }) => {
    // Each episode gets 10 tracks from the genre-matched track library.
    // Track rows are in a space-y-1 container. Each row has a track number.
    const trackRows = page.locator("text=/^\\d+\\./");
    const count = await trackRows.count();
    expect(count).toBe(10);
  });

  test("track rows show title and artist", async ({ page }) => {
    // The track list container is div.space-y-1. Each direct child is a
    // track row with: track number div, then a .flex-grow div containing
    // a .font-medium title and a .text-sm artist.
    const firstRow = page.locator(".space-y-1 > div").first();
    const title = firstRow.locator(".font-medium");
    const artist = firstRow.locator(".flex-grow .text-sm");

    await expect(title).toBeVisible();
    await expect(artist).toBeVisible();

    // Title is quoted: e.g. "Birdland"
    const titleText = await title.textContent();
    expect(titleText).toMatch(/^".+"$/);
  });

  test("track numbers are sequential", async ({ page }) => {
    // Verify tracks are numbered 1-10.
    for (let i = 1; i <= 10; i++) {
      await expect(page.getByText(`${i}.`, { exact: true })).toBeAttached();
    }
  });
});
