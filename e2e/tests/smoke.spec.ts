import { test, expect } from "@playwright/test";

test.describe("Smoke tests", () => {
  test("homepage loads", async ({ page }) => {
    await page.goto("/");
    await expect(page).toHaveTitle(/KPBJ/i);
  });

  test("shows page loads", async ({ page }) => {
    const response = await page.goto("/shows");
    expect(response?.status()).toBe(200);
    await expect(page.locator("body")).not.toBeEmpty();
  });

  test("navigation via HTMX works", async ({ page }) => {
    await page.goto("/");

    // Click a navigation link and verify the page content updates.
    const showsLink = page.locator('a[href*="/shows"]').first();
    if (await showsLink.isVisible()) {
      await showsLink.click();
      await page.waitForURL(/\/shows/);
      await expect(page.locator("body")).not.toBeEmpty();
    }
  });
});
