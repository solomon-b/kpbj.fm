import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Public page load tests
// ---------------------------------------------------------------------------
// These verify that the public pages with no spec of their own return a 200,
// and that missing content renders the 404 page. The specs of the other
// pages (shows, schedule, events, blog, store) load those pages already.

test.describe("Public pages", () => {
  test("static pages return 200", async ({ page }) => {
    // page.request sends plain HTTP requests, so no page has to render.
    for (const url of ["/about", "/donate", "/privacy-policy", "/terms-of-service"]) {
      const response = await page.request.get(url);
      expect(response.status(), url).toBe(200);
    }
  });

  // -------------------------------------------------------------------------
  // 404 error pages
  // -------------------------------------------------------------------------

  test("missing content renders the 404 page", async ({ page }) => {
    await page.goto("/shows/does-not-exist");
    await expect(page.locator("h1", { hasText: "Show not found" })).toBeVisible();
    await expect(
      page.getByText("The page you're looking for doesn't exist or has been removed.")
    ).toBeVisible();

    for (const url of ["/blog/99999/does-not-exist", "/events/99999/does-not-exist"]) {
      await page.goto(url);
      await expect(page.locator("h1", { hasText: "not found" }), url).toBeVisible();
      await expect(page.getByText("Back to home"), url).toBeVisible();
    }
  });

  test("404 back to home link navigates to homepage", async ({ page }) => {
    await page.goto("/shows/does-not-exist");
    await page.getByText("Back to home").click();
    await expect(page).toHaveURL("/");
  });
});
