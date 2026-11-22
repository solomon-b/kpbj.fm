import { test, expect } from "@playwright/test";
import { ADMIN_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Users CRUD tests
//
// Covers list, detail, edit, role change, and suspend/unsuspend for the users
// dashboard. Uses ADMIN_AUTH (only admins can change roles).
//
// Suspend/unsuspend tests use musicfan@example.com (expendable mock user).
// Role change tests use listener@example.com and restore the original role.
// User delete is skipped — too destructive for shared mock data.
// ---------------------------------------------------------------------------

test.use({ storageState: ADMIN_AUTH });

test.describe("Users CRUD", () => {
  // -------------------------------------------------------------------------
  // List
  // -------------------------------------------------------------------------

  test("list page loads with users table", async ({ page }) => {
    await page.goto("/dashboard/users");
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
  // Role change (on the edit form, Admin only)
  // -------------------------------------------------------------------------

  test("detail page shows the role as text", async ({ page }) => {
    await page.goto("/dashboard/users");
    const row = page.locator("#users-table-body tr", {
      hasText: "listener@example.com",
    });
    await row.locator("td").first().click();
    await page.waitForURL(/\/dashboard\/users\/\d+/);

    await expect(page.locator('#main-content select[name="role"]')).toHaveCount(0);
  });

  test("admin changes the role on the edit form and restores it", async ({ page }) => {
    // Change the role of listener@example.com to the given role.
    const setRole = async (role: string) => {
      await page.goto("/dashboard/users");
      const row = page.locator("#users-table-body tr", {
        hasText: "listener@example.com",
      });
      await row.locator("select").selectOption("edit");
      await page.waitForURL(/\/edit/);

      await page.locator('select[name="role"]').selectOption(role);
      await page.getByRole("button", { name: "SAVE CHANGES" }).click();
      await page.waitForURL(/\/dashboard\/users\/\d+$/);
      const roleValue = page
        .locator("dt", { hasText: /^Role$/ })
        .locator("xpath=following-sibling::dd[1]");
      await expect(roleValue).toHaveText(role);
    };

    await setRole("Host");
    await setRole("User");
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
