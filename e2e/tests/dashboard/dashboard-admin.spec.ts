import { test, expect } from "@playwright/test";
import { ADMIN_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Admin dashboard tests
//
// Verifies page structure, sidebar navigation, role-specific items, show
// selector, and navigation links for the Admin role.
//
// Admin sees all shows (via getAllActiveShows). We navigate directly to a
// known show slug to avoid the slow double meta-refresh redirect chain.
// ---------------------------------------------------------------------------

test.use({ storageState: ADMIN_AUTH });

test.describe("Admin dashboard", () => {
  test.beforeEach(async ({ page }) => {
    // Navigate directly to a show-scoped page to skip the double
    // meta-refresh chain (/dashboard → /dashboard/episodes → /dashboard/episodes/{slug}).
    // "acoustic-sessions" is alphabetically first among mock shows.
    await page.goto("/dashboard/episodes/acoustic-sessions");
  });

  test("renders the frame and the full sidebar", async ({ page }) => {
    await expect(page).toHaveTitle("Dashboard | KPBJ 95.9FM");
    await expect(page.locator("aside")).toBeVisible();
    await expect(page.locator("#main-content")).toBeVisible();
    await expect(page.locator("#banner-container")).toBeAttached();
    await expect(page.getByRole("link", { name: "Back to Site" })).toBeVisible();

    const sidebar = page.locator("aside");
    await expect(sidebar.getByText("KPBJ 95.9FM")).toBeVisible();
    await expect(sidebar.getByText("Dashboard")).toBeVisible();

    await expect(sidebar.getByText("HOST", { exact: true })).toBeVisible();
    await expect(sidebar.getByText("ADMIN", { exact: true })).toBeVisible();
    await expect(sidebar.getByRole("link", { name: "EPISODES", exact: true })).toBeVisible();
    // The exact regex distinguishes SHOWS from "SHOW SETTINGS".
    for (const name of [
      "SHOW SETTINGS",
      "STATION IDS",
      "EPHEMERAL",
      "USERS",
      /^SHOWS$/,
      "STATION BLOG",
      "EVENTS",
      "MISSING EPISODES",
      "SITE PAGES",
      "STREAM",
    ]) {
      await expect(sidebar.getByRole("link", { name }), String(name)).toBeVisible();
    }

    // Display name "Admin" shown in bold, and the role in a .text-xs span
    // inside the user info section at the bottom.
    await expect(sidebar.getByText("Admin", { exact: true }).first()).toBeVisible();
    await expect(
      sidebar.locator(".mt-auto").locator("span.text-xs", { hasText: /^Admin$/ })
    ).toBeVisible();
    await expect(sidebar.getByRole("link", { name: "Settings", exact: true })).toBeVisible();
    await expect(sidebar.getByRole("link", { name: "Logout" })).toBeVisible();
  });

  test("show selector lists the shows and navigates to the selected one", async ({ page }) => {
    // Admin has access to all shows, so the selector has many options.
    const selector = page.locator("#show-selector");
    await expect(selector).toBeVisible();
    expect(await selector.locator("option").count()).toBeGreaterThan(1);

    await selector.selectOption("sunday-morning-jazz");
    await page.waitForURL(/\/dashboard\/episodes\/sunday-morning-jazz/);
  });

  test("sidebar links navigate to their pages", async ({ page }) => {
    // The sidebar stays in place after each HTMX swap, so the test clicks
    // the links one after another.
    const sidebar = page.locator("aside");
    const pages: [string | RegExp, RegExp][] = [
      ["USERS", /\/dashboard\/users/],
      [/^SHOWS$/, /\/dashboard\/shows/],
      ["EVENTS", /\/dashboard\/events/],
      ["STATION BLOG", /\/dashboard\/station-blog/],
      ["STREAM", /\/dashboard\/stream-settings/],
    ];
    for (const [name, url] of pages) {
      await test.step(String(name), async () => {
        await sidebar.getByRole("link", { name }).click();
        await page.waitForURL(url);
      });
    }
  });
});
