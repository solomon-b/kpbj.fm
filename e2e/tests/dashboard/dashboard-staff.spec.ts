import { test, expect } from "@playwright/test";
import { STAFF_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Staff dashboard tests
//
// Verifies that Staff users see the HOST section, ADMIN section (without
// STREAM), and correct user info.
//
// Staff users are NOT in the show_hosts table, so they have no shows.
// We navigate to /dashboard/users (a non-show-scoped page) to test the
// sidebar layout. EPISODES and SHOW SETTINGS render as disabled <span>
// elements (not <a> links) when no show is selected.
// ---------------------------------------------------------------------------

test.use({ storageState: STAFF_AUTH });

test.describe("Staff dashboard", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/dashboard/users");
  });

  // -------------------------------------------------------------------------
  // HOST section
  // -------------------------------------------------------------------------

  test("sidebar has HOST label", async ({ page }) => {
    await expect(
      page.locator("aside").getByText("HOST", { exact: true })
    ).toBeVisible();
  });

  test("HOST section has EPISODES as disabled span (no show selected)", async ({ page }) => {
    // Without a selected show, EPISODES renders as a disabled <span> (not a link).
    await expect(
      page.locator("aside").getByText("EPISODES", { exact: true })
    ).toBeVisible();
    await expect(
      page.locator("aside").getByRole("link", { name: "EPISODES", exact: true })
    ).not.toBeAttached();
  });

  test("HOST section has SHOW SETTINGS as disabled span (no show selected)", async ({ page }) => {
    // Without a selected show, SHOW SETTINGS renders as a disabled <span>.
    await expect(
      page.locator("aside").getByText("SHOW SETTINGS", { exact: true })
    ).toBeVisible();
    await expect(
      page.locator("aside").getByRole("link", { name: "SHOW SETTINGS" })
    ).not.toBeAttached();
  });

  test("HOST section has STATION IDS link", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "STATION IDS" })
    ).toBeVisible();
  });

  test("HOST section has EPHEMERAL link", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "EPHEMERAL" })
    ).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // ADMIN section
  // -------------------------------------------------------------------------

  test("sidebar has ADMIN label", async ({ page }) => {
    await expect(
      page.locator("aside").getByText("ADMIN", { exact: true })
    ).toBeVisible();
  });

  test("ADMIN section has USERS link", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "USERS" })
    ).toBeVisible();
  });

  test("ADMIN section has SHOWS link", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: /^SHOWS$/ })
    ).toBeVisible();
  });

  test("ADMIN section has STATION BLOG link", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "STATION BLOG" })
    ).toBeVisible();
  });

  test("ADMIN section has EVENTS link", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "EVENTS" })
    ).toBeVisible();
  });

  test("ADMIN section has MISSING EPISODES link", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "MISSING EPISODES" })
    ).toBeVisible();
  });

  test("ADMIN section has SITE PAGES link", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "SITE PAGES" })
    ).toBeVisible();
  });

  test("STREAM is not visible for Staff role", async ({ page }) => {
    // STREAM is admin-only — guarded by `when (isAdmin ...)`.
    await expect(
      page.locator("aside").getByRole("link", { name: "STREAM" })
    ).not.toBeAttached();
  });

  // -------------------------------------------------------------------------
  // User info
  // -------------------------------------------------------------------------

  test("sidebar shows Staff Member display name", async ({ page }) => {
    await expect(
      page.locator("aside").getByText("Staff Member", { exact: true })
    ).toBeVisible();
  });

  test("sidebar shows Staff role", async ({ page }) => {
    const userInfo = page.locator("aside .mt-auto");
    await expect(userInfo.locator("span.text-xs", { hasText: /^Staff$/ })).toBeVisible();
  });
});
