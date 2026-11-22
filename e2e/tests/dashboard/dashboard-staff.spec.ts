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
// sidebar layout.
// ---------------------------------------------------------------------------

test.use({ storageState: STAFF_AUTH });

test.describe("Staff dashboard", () => {
  test("sidebar shows the HOST and ADMIN sections without STREAM", async ({ page }) => {
    await page.goto("/dashboard/users");
    const sidebar = page.locator("aside");

    await expect(sidebar.getByText("HOST", { exact: true })).toBeVisible();

    // The users page is not about a show, so EPISODES and SHOW SETTINGS go
    // to the current show route.
    await expect(
      sidebar.getByRole("link", { name: "EPISODES", exact: true })
    ).toHaveAttribute("href", "/dashboard/current-show/episodes");
    await expect(
      sidebar.getByRole("link", { name: "SHOW SETTINGS" })
    ).toHaveAttribute("href", "/dashboard/current-show/settings");

    await expect(sidebar.getByText("ADMIN", { exact: true })).toBeVisible();
    // The exact regex distinguishes SHOWS from "SHOW SETTINGS".
    for (const name of [
      "STATION IDS",
      "EPHEMERAL",
      "USERS",
      /^SHOWS$/,
      "STATION BLOG",
      "EVENTS",
      "MISSING EPISODES",
      "SITE PAGES",
    ]) {
      await expect(sidebar.getByRole("link", { name }), String(name)).toBeVisible();
    }

    // STREAM is admin-only. It is guarded by `when (isAdmin ...)`.
    await expect(sidebar.getByRole("link", { name: "STREAM" })).not.toBeAttached();

    await expect(sidebar.getByText("Staff Member", { exact: true })).toBeVisible();
    await expect(
      sidebar.locator(".mt-auto").locator("span.text-xs", { hasText: /^Staff$/ })
    ).toBeVisible();
  });
});
