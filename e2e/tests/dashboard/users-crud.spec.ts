import { test, expect } from "@playwright/test";
import { ADMIN_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Users CRUD tests
//
// Covers list, detail, inline role change, edit, and suspend/unsuspend for
// the users dashboard. Uses ADMIN_AUTH (only admins can manage users).
//
// Suspend/unsuspend tests use musicfan@example.com (expendable mock user).
// Role change tests use listener@example.com and restore the original role.
// User delete is skipped — too destructive for shared mock data.
//
// NOTE: The inline role change uses HTMX innerHTML swap which creates a
// nested duplicate ID (#role-dropdown-container). After the first swap,
// the inner <div> has the same ID as the outer <dd>. We use
// page.getByText("Saved") to avoid strict-mode violations on the ID.
// ---------------------------------------------------------------------------

test.use({ storageState: ADMIN_AUTH });

test.describe("Users CRUD", () => {
  // -------------------------------------------------------------------------
  // List
  // -------------------------------------------------------------------------

  test("list page loads with users table", async ({ page }) => {
    await page.goto("/dashboard/users");
    await expect(page.locator("header h1")).toHaveText("Users");
    await expect(page.locator("#users-table-body")).toBeVisible();
    const rows = page.locator("#users-table-body tr");
    await expect(rows.first()).toBeVisible();
  });

  test("list shows known mock users", async ({ page }) => {
    await page.goto("/dashboard/users");
    await expect(page.locator("#users-table-body")).toContainText(
      "listener@example.com"
    );
    await expect(page.locator("#users-table-body")).toContainText(
      "musicfan@example.com"
    );
  });

  // -------------------------------------------------------------------------
  // Detail (via table row click)
  // -------------------------------------------------------------------------

  test("clicking a row navigates to user detail", async ({ page }) => {
    await page.goto("/dashboard/users");
    const row = page.locator("#users-table-body tr", {
      hasText: "listener@example.com",
    });
    await row.locator("td").first().click();
    await page.waitForURL(/\/dashboard\/users\/\d+/);
    await expect(page.getByText("listener@example.com")).toBeVisible();
    await expect(page.getByText("RadioHead42")).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Inline role change (on detail page)
  // -------------------------------------------------------------------------

  test("inline role change updates and restores", async ({ page }) => {
    // Navigate to listener@example.com detail page.
    await page.goto("/dashboard/users");
    const row = page.locator("#users-table-body tr", {
      hasText: "listener@example.com",
    });
    await row.locator("td").first().click();
    await page.waitForURL(/\/dashboard\/users\/\d+/);

    // Use dd# to target the original container specifically.
    const roleSelect = page.locator(
      'dd#role-dropdown-container select[name="role"]'
    );
    await expect(roleSelect).toBeVisible();

    // Change User → Host.
    await roleSelect.selectOption("Host");
    // HTMX patches and re-renders. The "Saved" text appears in the response.
    // After innerHTML swap, there's a nested duplicate ID, so use getByText.
    await expect(page.getByText("Saved").first()).toBeVisible();

    // Restore Host → User.
    // After the swap, there are two select[name="role"] on the page (one in
    // the header, one in the detail section). Scope to #main-content.
    const restoredSelect = page.locator(
      '#main-content select[name="role"]'
    );
    await restoredSelect.selectOption("User");
    await expect(page.getByText("Saved").first()).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Edit form
  // -------------------------------------------------------------------------

  test("edit form loads and saves changes", async ({ page }) => {
    await page.goto("/dashboard/users");
    const row = page.locator("#users-table-body tr", {
      hasText: "listener@example.com",
    });
    await row.locator("select").selectOption("edit");
    await page.waitForURL(/\/edit/);

    // Verify form is pre-filled.
    await expect(page.locator('input[name="display_name"]')).toHaveValue(
      "RadioHead42"
    );

    // Submit without changes.
    await page.getByRole("button", { name: "SAVE CHANGES" }).click();
    await page.waitForURL(/\/dashboard\/users\/\d+$/);
    await expect(page.locator("#banner-container")).toContainText(
      /updated|Updated/i
    );
  });

  // -------------------------------------------------------------------------
  // Suspend / Unsuspend (serial, uses musicfan@example.com)
  // -------------------------------------------------------------------------

  test.describe.serial("suspend and unsuspend user", () => {
    test("suspend user", async ({ page }) => {
      await page.goto("/dashboard/users");
      const row = page.locator("#users-table-body tr", {
        hasText: "musicfan@example.com",
      });
      await expect(row).toBeVisible();

      page.once("dialog", (d) => d.accept());
      await row.locator("select").selectOption("suspend");

      // Row should update to show "Suspended" status.
      await expect(row).toContainText(/Suspended/);
      await expect(page.locator("#banner-container")).toContainText(
        /Suspended/i
      );
    });

    test("unsuspend user", async ({ page }) => {
      await page.goto("/dashboard/users");
      const row = page.locator("#users-table-body tr", {
        hasText: "musicfan@example.com",
      });
      await expect(row).toBeVisible();

      page.once("dialog", (d) => d.accept());
      await row.locator("select").selectOption("unsuspend");

      // Row should update back to "Active" status.
      await expect(row).toContainText(/Active/);
      await expect(page.locator("#banner-container")).toContainText(
        /Unsuspended/i
      );
    });
  });
});
