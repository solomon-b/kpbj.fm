import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Show detail page (/shows/:slug)
// ---------------------------------------------------------------------------
// Tests use "Sunday Morning Jazz" as the fixture show because it has known
// mock data: host (Smooth Sullivan), tags (Jazz, Soul), description,
// logo image, schedule, and 3 episodes.

const SHOW_URL = "/shows/sunday-morning-jazz";

test.describe("Show detail", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto(SHOW_URL);
  });

  test("displays the show header and tags", async ({ page }) => {
    // The title renders in uppercase.
    await expect(page.locator("h1")).toContainText("SUNDAY MORNING JAZZ");
    await expect(page.getByText("Start your Sunday with smooth jazz classics")).toBeVisible();
    await expect(page.locator('img[alt="Sunday Morning Jazz logo"]')).toBeVisible();
    await expect(page.getByText("Smooth Sullivan")).toBeVisible();

    // Schedule renders as "Sundays <time range>" (no "Every" prefix when
    // the show airs all 5 weeks).
    await expect(page.getByText(/Sundays/)).toBeVisible();

    await expect(page.getByText("#Jazz")).toBeVisible();
    await expect(page.getByText("#Soul")).toBeVisible();
  });

  test("displays the episode cards", async ({ page }) => {
    // Scope to #main-content to avoid matching the navbar player.
    const episodes = page.locator("#main-content [x-data*=playerId]");
    await expect(episodes).toHaveCount(3);

    const first = episodes.first();
    await expect(first.getByText(/\w+ \d{1,2}, \d{4}/)).toBeVisible();
    await expect(first.locator("button[x-on\\:click='toggle()']")).toBeVisible();

    // 3 episodes fit on one page of 10, so there is no Next link.
    await expect(page.locator("a", { hasText: "Next" })).not.toBeAttached();
  });

  test("clicking episode artwork navigates to episode detail", async ({ page }) => {
    // The artwork is wrapped in an <a> with an href to the episode page.
    await page.locator("#main-content [x-data*=playerId]").first().locator("a").first().click();
    await expect(page).toHaveURL(/\/shows\/sunday-morning-jazz\/episodes\//);
  });

  test("clicking a tag shows the shows with that tag", async ({ page }) => {
    // Mock data has 5 shows tagged "Jazz".
    await page.getByText("#Jazz").click();
    await page.waitForURL(/\/shows\?.*tag=/);
    await expect(page.locator("#shows-list > a")).toHaveCount(5);
  });
});
