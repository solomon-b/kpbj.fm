import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Public page load tests
// ---------------------------------------------------------------------------
// These verify that every public page returns a 200 and renders content.
// They catch server errors, broken templates, and missing database queries.

test.describe("Public pages", () => {
  // test.describe() groups related tests. They share a name in the report
  // and you can apply shared config to the group (timeouts, tags, etc.).

  test("homepage", async ({ page }) => {
    await page.goto("/");
    await expect(page).toHaveTitle(/KPBJ/i);
  });

  test("shows", async ({ page }) => {
    // page.goto() returns the HTTP response, so we can check the status.
    const response = await page.goto("/shows");
    expect(response?.status()).toBe(200);
  });

  test("schedule", async ({ page }) => {
    const response = await page.goto("/schedule");
    expect(response?.status()).toBe(200);
  });

  test("events", async ({ page }) => {
    const response = await page.goto("/events");
    expect(response?.status()).toBe(200);
  });

  test("about", async ({ page }) => {
    const response = await page.goto("/about");
    expect(response?.status()).toBe(200);
  });

  test("donate", async ({ page }) => {
    const response = await page.goto("/donate");
    expect(response?.status()).toBe(200);
  });

  test("privacy policy", async ({ page }) => {
    const response = await page.goto("/privacy-policy");
    expect(response?.status()).toBe(200);
  });

  test("terms of service", async ({ page }) => {
    const response = await page.goto("/terms-of-service");
    expect(response?.status()).toBe(200);
  });
});
