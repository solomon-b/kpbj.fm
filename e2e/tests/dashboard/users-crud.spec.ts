import { test, expect, type Page } from "@playwright/test";
import { ADMIN_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Users CRUD tests
//
// Covers list, detail, inline role change, edit, and suspend/unsuspend for
// the users dashboard. Uses ADMIN_AUTH (only admins can manage users).
//
// Suspend/unsuspend tests use musicfan@example.com (expendable mock user).
// Role change tests use vinyl.collector@example.com and restore the
// original role. They do not use listener@example.com, because USER_AUTH
// logs in as that user and other specs run at the same time.
// User delete is skipped — too destructive for shared mock data.
//
// NOTE: The inline role change uses HTMX innerHTML swap which creates a
// nested duplicate ID (#role-dropdown-container). After the first swap,
// the inner <div> has the same ID as the outer <dd>. We use
// page.getByText("Saved") to avoid strict-mode violations on the ID.
// ---------------------------------------------------------------------------

test.use({ storageState: ADMIN_AUTH });

function userRow(page: Page, email: string) {
  return page.locator("#users-table-body tr", { hasText: email });
}

// Open the detail page of a user from the list. The search keeps the user
// on the first page of 20 rows.
async function openUser(page: Page, email: string) {
  await page.goto(`/dashboard/users?q=${encodeURIComponent(email)}`);
  await userRow(page, email).locator("td").first().click();
  await page.waitForURL(/\/dashboard\/users\/\d+/);
}

test.describe("Users CRUD", () => {
  // -------------------------------------------------------------------------
  // List and detail
  // -------------------------------------------------------------------------

  test("list shows the mock users and a row opens the detail page", async ({ page }) => {
    await page.goto("/dashboard/users");
    const tableBody = page.locator("#users-table-body");
    await expect(tableBody).toContainText("listener@example.com");
    await expect(tableBody).toContainText("musicfan@example.com");

    await openUser(page, "listener@example.com");
    await expect(page.getByText("listener@example.com")).toBeVisible();
    await expect(page.getByText("RadioHead42")).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Inline role change (on detail page)
  // -------------------------------------------------------------------------

  test("inline role change updates and restores", async ({ page }) => {
    await openUser(page, "vinyl.collector@example.com");

    // Use dd# to target the original container specifically.
    const roleSelect = page.locator('dd#role-dropdown-container select[name="role"]');
    await expect(roleSelect).toBeVisible();

    // Change User → Host.
    await roleSelect.selectOption("Host");
    // HTMX patches and re-renders. The "Saved" text appears in the response.
    // After innerHTML swap, there's a nested duplicate ID, so use getByText.
    await expect(page.getByText("Saved").first()).toBeVisible();

    // Restore Host → User.
    // After the swap, there are two select[name="role"] on the page (one in
    // the header, one in the detail section). Scope to #main-content.
    await page.locator('#main-content select[name="role"]').selectOption("User");
    await expect(page.getByText("Saved").first()).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Edit form
  // -------------------------------------------------------------------------

  test("edit form loads and saves changes", async ({ page }) => {
    await page.goto("/dashboard/users");
    await userRow(page, "listener@example.com").locator("select").selectOption("edit");
    await page.waitForURL(/\/edit/);

    await expect(page.locator('input[name="display_name"]')).toHaveValue("RadioHead42");

    // Submit without changes.
    await page.getByRole("button", { name: "SAVE CHANGES" }).click();
    await page.waitForURL(/\/dashboard\/users\/\d+$/);
    await expect(page.locator("#banner-container")).toContainText(/updated|Updated/i);
  });

  // -------------------------------------------------------------------------
  // Suspend / Unsuspend (uses musicfan@example.com)
  // -------------------------------------------------------------------------

  test("suspend and unsuspend user", async ({ page }) => {
    await page.goto("/dashboard/users");
    const row = userRow(page, "musicfan@example.com");
    await expect(row).toBeVisible();

    await test.step("suspend", async () => {
      page.once("dialog", (d) => d.accept());
      await row.locator("select").selectOption("suspend");
      await expect(row).toContainText(/Suspended/);
      await expect(page.locator("#banner-container")).toContainText(/Suspended/i);
    });

    await test.step("unsuspend", async () => {
      // Reload so the row has the actions of a suspended user.
      await page.goto("/dashboard/users");
      page.once("dialog", (d) => d.accept());
      await row.locator("select").selectOption("unsuspend");
      await expect(row).toContainText(/Active/);
      await expect(page.locator("#banner-container")).toContainText(/Unsuspended/i);
    });
  });
});
