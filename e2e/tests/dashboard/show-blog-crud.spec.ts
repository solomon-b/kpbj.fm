import { test, expect } from "@playwright/test";
import { HOST_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Show Blog CRUD tests
//
// Covers list, create, edit, and delete for show-scoped blog posts.
// Uses HOST_AUTH (host-sunday-morning-jazz@kpbj.fm).
//
// Each row has an Edit link and a Delete button. The Delete button has
// hx-confirm, which triggers a native confirm dialog.
//
// NOTE: Mock data has no show blog posts (show_blog_posts is truncated and
// not re-seeded). So one test creates a post, then edits and deletes it.
// ---------------------------------------------------------------------------

test.use({ storageState: HOST_AUTH });

const showSlug = "sunday-morning-jazz";
const LIST_URL = `/dashboard/blog/${showSlug}`;
const DETAIL_URL = new RegExp(`/dashboard/blog/${showSlug}/\\d+`);

test.describe("Show Blog CRUD", () => {
  test("create, edit, and delete show blog post", async ({ page }) => {
    const postTitle = `E2E Show Post ${Date.now()}`;
    const row = page.locator("#blog-posts-table-body tr", { hasText: postTitle });

    await test.step("create post via form", async () => {
      await page.goto(`${LIST_URL}/new`);
      await expect(page.getByText("NEW BLOG POST")).toBeVisible();

      await page.locator('input[name="title"]').fill(postTitle);
      await page
        .locator('textarea[name="content"]')
        .fill("This is an end-to-end test show blog post created by Playwright.");
      await page.locator('input[name="tags"]').fill("e2e-test, playwright");

      await page.getByRole("button", { name: "SAVE POST" }).click();
      await page.waitForURL(DETAIL_URL);
      await expect(page.locator("#banner-container")).toContainText(/created/i);
      await expect(page.getByText(postTitle)).toBeVisible();
    });

    await test.step("edit created post from list", async () => {
      await page.goto(LIST_URL);
      await row.getByRole("link", { name: "Edit", exact: true }).click();
      await page.waitForURL(/\/edit/);
      await expect(page.locator('input[name="title"]')).toHaveValue(postTitle);

      // Submit without changes.
      await page.getByRole("button", { name: "SAVE CHANGES" }).click();
      await page.waitForURL(DETAIL_URL);
      await expect(page.locator("#banner-container")).toContainText(/updated|Updated/i);
    });

    await test.step("delete created post from list", async () => {
      await page.goto(LIST_URL);
      await expect(row).toBeVisible();

      // The Delete button has hx-confirm, which fires a native dialog.
      page.once("dialog", (d) => d.accept());
      await row.getByRole("button", { name: "Delete", exact: true }).click();

      // Delete swaps outerHTML of the row, so the row disappears.
      await expect(row).not.toBeVisible();
    });
  });
});
