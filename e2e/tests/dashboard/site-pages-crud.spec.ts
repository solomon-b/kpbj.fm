import { test, expect } from "@playwright/test";
import { STAFF_AUTH, ADMIN_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Site Pages CRUD tests
//
// Site pages are fixed (no create/delete). Tests cover list, edit, history,
// and revision restore. Edit and history use STAFF_AUTH, restore uses
// ADMIN_AUTH (only admins can restore revisions).
//
// History and restore tests need at least one revision to exist. Since fresh
// mock data has no revisions, these tests perform an edit first to create one.
// ---------------------------------------------------------------------------

test.describe("Site Pages CRUD (Staff)", () => {
  test.use({ storageState: STAFF_AUTH });

  // -------------------------------------------------------------------------
  // List
  // -------------------------------------------------------------------------

  test("list page loads with pages table", async ({ page }) => {
    await page.goto("/dashboard/site-pages");
    await expect(page.locator("#site-pages-table-body")).toBeVisible();
    const rows = page.locator("#site-pages-table-body tr");
    await expect(rows.first()).toBeVisible();
  });

  test("list shows known pages", async ({ page }) => {
    await page.goto("/dashboard/site-pages");
    const tableBody = page.locator("#site-pages-table-body");
    await expect(tableBody).toContainText("About KPBJ");
    await expect(tableBody).toContainText("Privacy Policy");
    await expect(tableBody).toContainText("Terms of Service");
  });

  // -------------------------------------------------------------------------
  // Edit
  // -------------------------------------------------------------------------

  test("edit form loads pre-filled for About page", async ({ page }) => {
    await page.goto("/dashboard/site-pages");
    const row = page.locator("#site-pages-table-body tr", {
      hasText: "About KPBJ",
    });
    // Navigate to edit via actions dropdown.
    await row.locator("select").selectOption("edit");
    await page.waitForURL(/\/dashboard\/site-pages\/about\/edit/);

    // Verify form fields are pre-filled.
    await expect(page.locator('input[name="title"]')).toHaveValue(
      "About KPBJ"
    );
    const content = page.locator('textarea[name="content"]');
    await expect(content).toBeVisible();
    const contentValue = await content.inputValue();
    expect(contentValue).toContain("KPBJ");
  });

  test("edit form saves with content change", async ({ page }) => {
    await page.goto("/dashboard/site-pages/about/edit");

    // The handler only creates a revision if title or content actually change.
    // Append a space to the content to trigger a real update.
    const content = page.locator('textarea[name="content"]');
    const currentContent = await content.inputValue();
    await content.fill(currentContent + " ");

    // Add an edit summary.
    await page
      .locator('input[name="edit_summary"]')
      .fill("E2E test edit");

    // Submit the form.
    await page.getByRole("button", { name: "SAVE CHANGES" }).click();
    await page.waitForURL(/\/dashboard\/site-pages/);
    await expect(page.locator("#banner-container")).toContainText(
      /updated|Updated/i
    );
  });

  // -------------------------------------------------------------------------
  // History (self-contained — creates a revision first)
  // -------------------------------------------------------------------------

  test("history page shows revisions after edit", async ({ page }) => {
    // Create a revision by performing an edit on the Privacy Policy page.
    // The handler only creates a revision if title or content actually change.
    await page.goto("/dashboard/site-pages/privacy-policy/edit");
    const content = page.locator('textarea[name="content"]');
    const currentContent = await content.inputValue();
    await content.fill(currentContent + "\n\n<!-- E2E history test -->");
    await page
      .locator('input[name="edit_summary"]')
      .fill("E2E revision for history test");
    await page.getByRole("button", { name: "SAVE CHANGES" }).click();
    await page.waitForURL(/\/dashboard\/site-pages/);

    // Now navigate to history for the Privacy Policy page.
    await page.goto("/dashboard/site-pages/privacy-policy/history");
    await expect(page.locator("#revisions-table-body")).toBeVisible();
    const revisionRows = page.locator("#revisions-table-body tr");
    await expect(revisionRows.first()).toBeVisible();
  });
});

test.describe("Site Pages CRUD (Admin)", () => {
  test.use({ storageState: ADMIN_AUTH });

  // -------------------------------------------------------------------------
  // Revision restore (admin only, self-contained)
  // -------------------------------------------------------------------------

  test("restore revision from detail page", async ({ page }) => {
    // Create a revision by editing the Terms of Service page.
    // The handler only creates a revision if title or content actually change.
    await page.goto("/dashboard/site-pages/terms-of-service/edit");
    const content = page.locator('textarea[name="content"]');
    const currentContent = await content.inputValue();
    await content.fill(currentContent + "\n\n<!-- E2E restore test -->");
    await page
      .locator('input[name="edit_summary"]')
      .fill("E2E revision for restore test");
    await page.getByRole("button", { name: "SAVE CHANGES" }).click();
    await page.waitForURL(/\/dashboard\/site-pages/);

    // Navigate to history.
    await page.goto("/dashboard/site-pages/terms-of-service/history");
    await expect(page.locator("#revisions-table-body")).toBeVisible();

    // Click the first revision row to view its detail.
    const firstRevision = page
      .locator("#revisions-table-body tr")
      .first();
    await firstRevision.locator("td").first().click();
    await page.waitForURL(
      /\/dashboard\/site-pages\/terms-of-service\/revisions\/\d+/
    );

    // Click RESTORE REVISION — has a confirm dialog.
    page.once("dialog", (d) => d.accept());
    await page
      .getByRole("button", { name: "RESTORE REVISION" })
      .click();

    // Should redirect to history with a success banner.
    await page.waitForURL(
      /\/dashboard\/site-pages\/terms-of-service\/history/
    );
    await expect(page.locator("#banner-container")).toContainText(
      /restored|Restored/i
    );
  });
});
