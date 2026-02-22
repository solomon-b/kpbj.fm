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

  // -------------------------------------------------------------------------
  // Page structure
  // -------------------------------------------------------------------------

  test("has the correct title", async ({ page }) => {
    await expect(page).toHaveTitle("Dashboard | KPBJ 95.9FM");
  });

  test("has sidebar, main content, and banner container", async ({ page }) => {
    await expect(page.locator("aside")).toBeVisible();
    await expect(page.locator("#main-content")).toBeVisible();
    await expect(page.locator("#banner-container")).toBeAttached();
  });

  // -------------------------------------------------------------------------
  // Sidebar branding
  // -------------------------------------------------------------------------

  test("sidebar shows KPBJ branding and Dashboard label", async ({ page }) => {
    const sidebar = page.locator("aside");
    await expect(sidebar.getByText("KPBJ 95.9FM")).toBeVisible();
    await expect(sidebar.getByText("Dashboard")).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // HOST section
  // -------------------------------------------------------------------------

  test("sidebar has HOST label", async ({ page }) => {
    await expect(
      page.locator("aside").getByText("HOST", { exact: true })
    ).toBeVisible();
  });

  test("HOST section has EPISODES link", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "EPISODES", exact: true })
    ).toBeVisible();
  });

  test("HOST section has SHOW SETTINGS link", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "SHOW SETTINGS" })
    ).toBeVisible();
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

  test("ADMIN section has STREAM link (admin-only)", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "STREAM" })
    ).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // User info footer
  // -------------------------------------------------------------------------

  test("sidebar shows Admin display name and role", async ({ page }) => {
    const sidebar = page.locator("aside");
    // Display name "Admin" shown in bold.
    await expect(sidebar.getByText("Admin", { exact: true }).first()).toBeVisible();
    // Role rendered in a .text-xs span inside the user info section at bottom.
    const userInfo = sidebar.locator(".mt-auto");
    await expect(userInfo.locator("span.text-xs", { hasText: /^Admin$/ })).toBeVisible();
  });

  test("sidebar has Settings and Logout links", async ({ page }) => {
    const sidebar = page.locator("aside");
    await expect(sidebar.getByRole("link", { name: "Settings", exact: true })).toBeVisible();
    await expect(sidebar.getByRole("link", { name: "Logout" })).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Top bar
  // -------------------------------------------------------------------------

  test("top bar shows page title", async ({ page }) => {
    await expect(page.locator("header h1")).toBeVisible();
  });

  test("top bar has Back to Site link", async ({ page }) => {
    await expect(page.getByRole("link", { name: "Back to Site" })).toBeVisible();
  });

  test("top bar has show selector with multiple options", async ({ page }) => {
    const selector = page.locator("#show-selector");
    await expect(selector).toBeVisible();
    // Admin has access to all shows — should have multiple options.
    const optionCount = await selector.locator("option").count();
    expect(optionCount).toBeGreaterThan(1);
  });

  test("changing show selector navigates to the selected show", async ({ page }) => {
    const selector = page.locator("#show-selector");
    // Pick a different show from the dropdown. "sunday-morning-jazz" is
    // guaranteed to exist and is different from "acoustic-sessions".
    await selector.selectOption("sunday-morning-jazz");
    await page.waitForURL(/\/dashboard\/episodes\/sunday-morning-jazz/);
  });

  // -------------------------------------------------------------------------
  // Navigation
  // -------------------------------------------------------------------------

  test("clicking USERS navigates to users page", async ({ page }) => {
    await page.locator("aside").getByRole("link", { name: "USERS" }).click();
    await page.waitForURL(/\/dashboard\/users/);
    await expect(page.locator("header h1")).toHaveText("Users");
  });

  test("clicking SHOWS navigates to shows page", async ({ page }) => {
    await page.locator("aside").getByRole("link", { name: /^SHOWS$/ }).click();
    await page.waitForURL(/\/dashboard\/shows/);
    await expect(page.locator("header h1")).toHaveText("Shows");
  });

  test("clicking EVENTS navigates to events page", async ({ page }) => {
    await page.locator("aside").getByRole("link", { name: "EVENTS" }).click();
    await page.waitForURL(/\/dashboard\/events/);
    await expect(page.locator("header h1")).toHaveText("Events");
  });

  test("clicking STATION BLOG navigates to station blog page", async ({ page }) => {
    await page.locator("aside").getByRole("link", { name: "STATION BLOG" }).click();
    await page.waitForURL(/\/dashboard\/station-blog/);
    await expect(page.locator("header h1")).toHaveText("Station Blog");
  });

  test("clicking STREAM navigates to stream settings page", async ({ page }) => {
    await page.locator("aside").getByRole("link", { name: "STREAM" }).click();
    await page.waitForURL(/\/dashboard\/stream-settings/);
    await expect(page.locator("header h1")).toHaveText("Stream Settings");
  });
});
