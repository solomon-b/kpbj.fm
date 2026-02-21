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

  // -------------------------------------------------------------------------
  // 404 error pages
  // -------------------------------------------------------------------------

  test("non-existent show renders a 404 page", async ({ page }) => {
    await page.goto("/shows/does-not-exist");
    await expect(
      page.locator("h1", { hasText: "Show not found" })
    ).toBeVisible();
    await expect(
      page.getByText("The page you're looking for doesn't exist or has been removed.")
    ).toBeVisible();
    await expect(page.getByText("Back to home")).toBeVisible();
  });

  test("non-existent blog post renders a 404 page", async ({ page }) => {
    await page.goto("/blog/99999/does-not-exist");
    await expect(
      page.locator("h1", { hasText: "not found" })
    ).toBeVisible();
    await expect(page.getByText("Back to home")).toBeVisible();
  });

  test("non-existent event renders a 404 page", async ({ page }) => {
    await page.goto("/events/99999/does-not-exist");
    await expect(
      page.locator("h1", { hasText: "not found" })
    ).toBeVisible();
    await expect(page.getByText("Back to home")).toBeVisible();
  });

  test("404 back to home link navigates to homepage", async ({ page }) => {
    await page.goto("/shows/does-not-exist");
    await page.getByText("Back to home").click();
    await expect(page).toHaveURL("/");
  });
});
