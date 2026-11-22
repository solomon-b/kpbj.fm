import { test, expect } from "@playwright/test";
import { CROSS_BROWSER } from "../playwright.config";

// ---------------------------------------------------------------------------
// Navigation tests
// ---------------------------------------------------------------------------
// These verify that clicking nav links loads the right pages via HTMX.
// The app uses hxGet_ + hxPushUrl_, so clicking a link:
//   1. Fires an XHR to fetch the HTML fragment
//   2. Swaps it into #main-content
//   3. Pushes the new URL to the browser history
//
// Desktop nav only. See mobile.spec.ts for the hamburger menu.

test.describe("Main navigation", () => {
  test("nav links load each section", { tag: CROSS_BROWSER }, async ({ page }) => {
    const sections: [string, RegExp][] = [
      ["Shows", /\/shows/],
      ["Schedule", /\/schedule/],
      ["Events", /\/events/],
      ["Store", /\/store/],
      ["About", /\/about/],
    ];

    for (const [name, url] of sections) {
      await test.step(name, async () => {
        await page.goto("/");
        await page.getByRole("link", { name }).first().click();
        await page.waitForURL(url);
      });
    }
  });

  test("the swap renders the new page content", async ({ page }) => {
    await page.goto("/");
    await page.getByRole("link", { name: "Shows" }).first().click();
    await page.waitForURL(/\/shows/);
    await expect(page.locator("#shows-list")).toBeVisible();

    await page.getByRole("link", { name: "Schedule" }).first().click();
    await page.waitForURL(/\/schedule/);
    await expect(page.getByRole("heading", { name: "Schedule" })).toBeVisible();
  });
});
