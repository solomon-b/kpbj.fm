import { test, expect, type Page } from "@playwright/test";
import { ADMIN_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Users CRUD tests
//
// Covers list, detail, edit, role change, and suspend/unsuspend for the users
// dashboard. Uses ADMIN_AUTH (only admins can change roles).
//
// Suspend/unsuspend tests use musicfan@example.com (expendable mock user).
// Role change tests use vinyl.collector@example.com and restore the
// original role. They do not use listener@example.com, because USER_AUTH
// logs in as that user and other specs run at the same time.
// User delete is skipped — too destructive for shared mock data.
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
  // Role change (on the edit form, Admin only)
  // -------------------------------------------------------------------------

  test("detail page shows the role as text", async ({ page }) => {
    await openUser(page, "vinyl.collector@example.com");
    await expect(page.locator('#main-content select[name="role"]')).toHaveCount(0);
  });

  test("admin changes the role on the edit form and restores it", async ({ page }) => {
    const email = "vinyl.collector@example.com";

    // Change the role of the user to the given role.
    const setRole = async (role: string) => {
      await page.goto(`/dashboard/users?q=${encodeURIComponent(email)}`);
      await userRow(page, email).locator("select").selectOption("edit");
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
