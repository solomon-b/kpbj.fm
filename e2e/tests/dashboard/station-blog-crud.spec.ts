import { test, expect, type Page } from "@playwright/test";
import { STAFF_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Station Blog CRUD tests
//
// Covers list, create, detail, edit, and delete for station-wide blog posts.
// Uses STAFF_AUTH (minimum role for station blog management).
// ---------------------------------------------------------------------------

test.use({ storageState: STAFF_AUTH });

const AWARD_POST = "KPBJ Wins 2025 Community Radio Excellence Award";

function postRow(page: Page, title: string) {
  return page.locator("#station-blog-table-body tr", { hasText: title });
}

test.describe("Station Blog CRUD", () => {
  test("list shows the mock post and a row opens the detail page", async ({ page }) => {
    await page.goto("/dashboard/station-blog");
    await expect(postRow(page, AWARD_POST)).toBeVisible();

    await postRow(page, AWARD_POST).locator("td").first().click();
    await page.waitForURL(/\/dashboard\/station-blog\/\d+\//);
    await expect(page.getByText(AWARD_POST)).toBeVisible();
    await expect(page.getByText("Staff Member", { exact: true })).toBeVisible();
  });

  test("edit form loads pre-filled and saves", async ({ page }) => {
    await page.goto("/dashboard/station-blog");
    await postRow(page, AWARD_POST).getByRole("link", { name: "Edit", exact: true }).click();
    await page.waitForURL(/\/edit/);
    await expect(page.locator('input[name="title"]')).toHaveValue(AWARD_POST);

    // Submit without changes.
    await page.getByRole("button", { name: "UPDATE POST" }).click();
    await page.waitForURL(/\/dashboard\/station-blog\/\d+\//);
    await expect(page.locator("#banner-container")).toContainText(/updated|Updated/i);
  });

  // One test, so the delete step cleans up what the create step made.
  test("create and delete station blog post", async ({ page }) => {
    const postTitle = `E2E Station Post ${Date.now()}`;

    await test.step("create post via form", async () => {
      await page.goto("/dashboard/station-blog/new");
      await expect(page.getByText("NEW BLOG POST")).toBeVisible();

      await page.locator('input[name="title"]').fill(postTitle);
      await page
        .locator('textarea[name="content"]')
        .fill("This is an end-to-end test blog post created by Playwright for CRUD testing.");
      await page.locator('input[name="tags"]').fill("e2e-test, playwright");

      await page.getByRole("button", { name: "SAVE POST" }).click();
      await page.waitForURL(/\/dashboard\/station-blog\/\d+\//);
      await expect(page.locator("#banner-container")).toContainText(/created/i);
      await expect(page.getByText(postTitle)).toBeVisible();
    });

    await test.step("delete from the list", async () => {
      await page.goto("/dashboard/station-blog");
      await expect(postRow(page, postTitle)).toBeVisible();

      page.once("dialog", (d) => d.accept());
      await postRow(page, postTitle).getByRole("button", { name: "Delete", exact: true }).click();

      await page.waitForURL(/\/dashboard\/station-blog/);
      await expect(page.locator("#banner-container")).toContainText(/deleted/i);
      await expect(
        page.locator("#station-blog-table-body").getByText(postTitle)
      ).not.toBeVisible();
    });
  });
});
