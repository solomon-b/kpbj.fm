import { test, expect } from "@playwright/test";
import { HOST_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Host dashboard tests
//
// Verifies that Host users see the HOST section but NOT the ADMIN section,
// have a single-show display (not a dropdown), and correct user info.
// ---------------------------------------------------------------------------

test.use({ storageState: HOST_AUTH });

test.describe("Host dashboard", () => {
  test.beforeEach(async ({ page }) => {
    // Navigate directly to the host's show to skip the meta-refresh chain.
    await page.goto("/dashboard/episodes/sunday-morning-jazz");
  });

  // -------------------------------------------------------------------------
  // HOST section
  // -------------------------------------------------------------------------

  test("sidebar has HOST label", async ({ page }) => {
    // Use exact: true to avoid matching "Host" role text.
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
  // ADMIN section should NOT be present
  // -------------------------------------------------------------------------

  test("sidebar does not have ADMIN label", async ({ page }) => {
    // "ADMIN" text should not appear in the sidebar for Host role.
    await expect(
      page.locator("aside").getByText("ADMIN", { exact: true })
    ).not.toBeAttached();
  });

  test("USERS link is not present", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "USERS" })
    ).not.toBeAttached();
  });

  test("admin SHOWS link is not present", async ({ page }) => {
    // Use exact regex to distinguish from "SHOW SETTINGS".
    await expect(
      page.locator("aside").getByRole("link", { name: /^SHOWS$/ })
    ).not.toBeAttached();
  });

  test("STATION BLOG link is not present", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "STATION BLOG" })
    ).not.toBeAttached();
  });

  test("STREAM link is not present", async ({ page }) => {
    await expect(
      page.locator("aside").getByRole("link", { name: "STREAM" })
    ).not.toBeAttached();
  });

  // -------------------------------------------------------------------------
  // Show selector — single show renders as text, not dropdown
  // -------------------------------------------------------------------------

  test("show selector shows single show as text", async ({ page }) => {
    // When a host has only one show, the selector renders as a <span>
    // instead of a <select> dropdown.
    await expect(page.locator("#show-selector")).not.toBeAttached();
    // Scope to the header to avoid matching episode description text.
    await expect(
      page.locator("header").getByText("Sunday Morning Jazz", { exact: true })
    ).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // User info
  // -------------------------------------------------------------------------

  test("sidebar shows Smooth Sullivan display name", async ({ page }) => {
    await expect(
      page.locator("aside").getByText("Smooth Sullivan", { exact: true })
    ).toBeVisible();
  });

  test("sidebar shows Host role", async ({ page }) => {
    // Scope to the user info footer section to avoid matching "HOST" label.
    const userInfo = page.locator("aside .mt-auto");
    await expect(userInfo.locator("span.text-xs", { hasText: /^Host$/ })).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // URL-based access denial — admin routes redirect Host away
  // -------------------------------------------------------------------------

  test("/dashboard/users redirects Host to dashboard home", async ({ page }) => {
    await page.goto("/dashboard/users");
    // Host gets NotAuthorized → redirected to /dashboard → meta-refresh to episodes.
    await page.waitForURL(/\/dashboard\/episodes\//);
  });

  test("/dashboard/shows redirects Host to dashboard home", async ({ page }) => {
    await page.goto("/dashboard/shows");
    await page.waitForURL(/\/dashboard\/episodes\//);
  });

  test("/dashboard/stream-settings redirects Host to dashboard home", async ({ page }) => {
    await page.goto("/dashboard/stream-settings");
    await page.waitForURL(/\/dashboard\/episodes\//);
  });
});
