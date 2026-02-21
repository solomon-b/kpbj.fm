import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Navigation tests
// ---------------------------------------------------------------------------
// These verify that clicking nav links loads the right pages via HTMX.
// Your app uses hxGet_ + hxPushUrl_ so clicking a link:
//   1. Fires an XHR to fetch the HTML fragment
//   2. Swaps it into #main-content
//   3. Pushes the new URL to the browser history
//
// Playwright sees the URL change and DOM update automatically.

test.describe("Main navigation", () => {
  // Desktop nav links are hidden on mobile (behind hamburger menu).
  // See mobile.spec.ts for mobile navigation tests.
  test.beforeEach(async ({}, testInfo) => {
    test.skip(testInfo.project.name.startsWith("mobile"), "Desktop nav — see mobile.spec.ts");
  });

  test("navigate to Shows from homepage", async ({ page }) => {
    await page.goto("/");

    // getByRole("link") finds <a> elements by their visible text.
    // Your nav has <a id="nav-shows">Shows</a>.
    await page.getByRole("link", { name: "Shows" }).first().click();

    // waitForURL accepts a string, regex, or predicate.
    // Using regex so it matches /shows, /shows?page=2, etc.
    await page.waitForURL(/\/shows/);

    // Verify the shows grid rendered after the HTMX swap.
    // locator("#shows-list") is a CSS selector — your shows grid has id="shows-list".
    await expect(page.locator("#shows-list")).toBeVisible();
  });

  test("navigate to Schedule from homepage", async ({ page }) => {
    await page.goto("/");
    await page.getByRole("link", { name: "Schedule" }).first().click();
    await page.waitForURL(/\/schedule/);

    // The schedule page has an h1 with "Schedule".
    await expect(page.getByRole("heading", { name: "Schedule" })).toBeVisible();
  });

  test("navigate to Events from homepage", async ({ page }) => {
    await page.goto("/");
    await page.getByRole("link", { name: "Events" }).first().click();
    await page.waitForURL(/\/events/);
  });

  test("navigate to About from homepage", async ({ page }) => {
    await page.goto("/");
    await page.getByRole("link", { name: "About" }).first().click();
    await page.waitForURL(/\/about/);
  });
});
