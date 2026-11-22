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

  test("displays the episode card, description, and tags", async ({ page }) => {
    const card = page.locator("#main-content [x-data*=playerId]").first();

    // The date is in a small muted div. The show title link above it also
    // has text-sm, so select the div only.
    await expect(
      page.locator("#main-content [x-data*=playerId] div.text-sm")
    ).toHaveText(/\w+ \d{1,2}, \d{4}/);
    await expect(card.locator("button[x-on\\:click='toggle()']")).toBeVisible();

    // Mock descriptions read "A great episode of Sunday Morning Jazz from <date>".
    await expect(page.getByText(/A great episode of Sunday Morning Jazz/)).toBeVisible();

    // Sunday Morning Jazz episodes always get the "classics" tag.
    await expect(page.getByText("#classics")).toBeVisible();
  });

  test("displays the track listing", async ({ page }) => {
    await expect(page.locator("h2", { hasText: "Track Listing" })).toBeVisible();

    // Each episode gets 10 tracks, numbered 1-10.
    await expect(page.locator("text=/^\\d+\\./")).toHaveCount(10);
    for (let i = 1; i <= 10; i++) {
      await expect(page.getByText(`${i}.`, { exact: true })).toBeAttached();
    }

    // Each row has a .font-medium title in quotes and a .text-sm artist.
    const firstRow = page.locator(".space-y-1 > div").first();
    await expect(firstRow.locator(".font-medium")).toHaveText(/^".+"$/);
    await expect(firstRow.locator(".flex-grow .text-sm")).toBeVisible();
  });

  test("clicking a tag navigates to the shows index", async ({ page }) => {
    await page.getByText("#classics").click();
    await expect(page).toHaveURL(/\/shows/);
  });
});
