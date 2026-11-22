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
  test("sidebar shows the HOST section, no ADMIN section, and the host", async ({ page }) => {
    // Navigate directly to the host's show to skip the meta-refresh chain.
    await page.goto("/dashboard/episodes/sunday-morning-jazz");
    const sidebar = page.locator("aside");

    // Use exact: true to avoid matching "Host" role text.
    await expect(sidebar.getByText("HOST", { exact: true })).toBeVisible();
    await expect(sidebar.getByRole("link", { name: "EPISODES", exact: true })).toBeVisible();
    for (const name of ["SHOW SETTINGS", "STATION IDS", "EPHEMERAL"]) {
      await expect(sidebar.getByRole("link", { name }), name).toBeVisible();
    }

    await expect(sidebar.getByText("ADMIN", { exact: true })).not.toBeAttached();
    // The exact regex distinguishes SHOWS from "SHOW SETTINGS".
    for (const name of ["USERS", /^SHOWS$/, "STATION BLOG", "STREAM"]) {
      await expect(sidebar.getByRole("link", { name }), String(name)).not.toBeAttached();
    }

    // With one show, the selector renders as a <span> and not a <select>.
    // Scope to the header to avoid matching episode description text.
    await expect(page.locator("#show-selector")).not.toBeAttached();
    await expect(
      page.locator("header").getByText("Sunday Morning Jazz", { exact: true })
    ).toBeVisible();

    await expect(sidebar.getByText("Smooth Sullivan", { exact: true })).toBeVisible();
    // Scope to the user info footer to avoid matching the "HOST" label.
    await expect(
      sidebar.locator(".mt-auto").locator("span.text-xs", { hasText: /^Host$/ })
    ).toBeVisible();
  });

  // App/Auth/GateSpec.hs covers the gate for each admin route. This test
  // checks that the browser follows the redirect chain.
  test("an admin page redirects Host to dashboard home", async ({ page }) => {
    await page.goto("/dashboard/users");
    // Host gets NotAuthorized → redirected to /dashboard → meta-refresh to episodes.
    await page.waitForURL(/\/dashboard\/episodes\//);
  });
});
