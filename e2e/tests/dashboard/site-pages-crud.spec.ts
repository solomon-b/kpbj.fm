import { test, expect, type Page } from "@playwright/test";
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

// Edit a site page so that it gets a new revision. The handler only creates
// a revision if title or content actually change, so this appends to the
// content.
async function editPage(page: Page, slug: string, suffix: string, summary: string) {
  await page.goto(`/dashboard/site-pages/${slug}/edit`);
  const content = page.locator('textarea[name="content"]');
  await content.fill((await content.inputValue()) + suffix);
  await page.locator('input[name="edit_summary"]').fill(summary);
  await page.getByRole("button", { name: "SAVE CHANGES" }).click();
  await page.waitForURL(/\/dashboard\/site-pages(?!.*\/edit)/);
}

test.describe("Site Pages CRUD (Staff)", () => {
  test.use({ storageState: STAFF_AUTH });

  test("list shows the known pages and the edit form loads pre-filled", async ({ page }) => {
    await page.goto("/dashboard/site-pages");
    const tableBody = page.locator("#site-pages-table-body");
    await expect(tableBody).toContainText("About KPBJ");
    await expect(tableBody).toContainText("Privacy Policy");
    await expect(tableBody).toContainText("Terms of Service");

    // Navigate to edit via actions dropdown.
    await tableBody.locator("tr", { hasText: "About KPBJ" }).locator("select").selectOption("edit");
    await page.waitForURL(/\/dashboard\/site-pages\/about\/edit/);
    await expect(page.locator('input[name="title"]')).toHaveValue("About KPBJ");
    await expect(page.locator('textarea[name="content"]')).toHaveValue(/KPBJ/);
  });

  test("edit form saves with content change", async ({ page }) => {
    await editPage(page, "about", " ", "E2E test edit");
    await expect(page.locator("#banner-container")).toContainText(/updated|Updated/i);
  });

  test("history page shows revisions after edit", async ({ page }) => {
    await editPage(
      page,
      "privacy-policy",
      "\n\n<!-- E2E history test -->",
      "E2E revision for history test",
    );

    await page.goto("/dashboard/site-pages/privacy-policy/history");
    await expect(page.locator("#revisions-table-body tr").first()).toBeVisible();
  });
});

test.describe("Site Pages CRUD (Admin)", () => {
  test.use({ storageState: ADMIN_AUTH });

  test("restore revision from detail page", async ({ page }) => {
    await editPage(
      page,
      "terms-of-service",
      "\n\n<!-- E2E restore test -->",
      "E2E revision for restore test",
    );

    // Click the first revision row to view its detail.
    await page.goto("/dashboard/site-pages/terms-of-service/history");
    await page.locator("#revisions-table-body tr").first().locator("td").first().click();
    await page.waitForURL(/\/dashboard\/site-pages\/terms-of-service\/revisions\/\d+/);

    // Click RESTORE REVISION — has a confirm dialog.
    page.once("dialog", (d) => d.accept());
    await page.getByRole("button", { name: "RESTORE REVISION" }).click();

    // Should redirect to history with a success banner.
    await page.waitForURL(/\/dashboard\/site-pages\/terms-of-service\/history/);
    await expect(page.locator("#banner-container")).toContainText(/restored|Restored/i);
  });
});
