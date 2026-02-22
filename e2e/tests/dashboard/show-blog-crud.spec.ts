import { test, expect } from "@playwright/test";
import { HOST_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Show Blog CRUD tests
//
// Covers list, create, edit, and delete for show-scoped blog posts.
// Uses HOST_AUTH (host-sunday-morning-jazz@kpbj.fm).
//
// The show blog uses a different dropdown pattern from ActionsDropdown:
// Alpine x-ref with hidden <a> (edit) and <button> (delete). The delete
// button has hx-confirm which triggers a native confirm dialog.
//
// NOTE: Mock data has no show blog posts (show_blog_posts is truncated and
// not re-seeded). All tests that need existing posts run in the serial
// block after the create test.
// ---------------------------------------------------------------------------

test.use({ storageState: HOST_AUTH });

const showSlug = "sunday-morning-jazz";

test.describe("Show Blog CRUD", () => {
  // -------------------------------------------------------------------------
  // List (may be empty — no show blog posts in mock data)
  // -------------------------------------------------------------------------

  test("list page loads", async ({ page }) => {
    await page.goto(`/dashboard/blog/${showSlug}`);
    // The page should load with the header visible, table may be empty.
    await expect(page.locator("header h1")).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Create → Edit → Delete (serial — creates data then cleans up)
  // -------------------------------------------------------------------------

  test.describe.serial("create, edit, and delete show blog post", () => {
    const postTitle = `E2E Show Post ${Date.now()}`;

    test("create post via form", async ({ page }) => {
      await page.goto(`/dashboard/blog/${showSlug}/new`);
      await expect(page.getByText("NEW BLOG POST")).toBeVisible();

      // Fill required fields.
      await page.locator('input[name="title"]').fill(postTitle);
      await page
        .locator('textarea[name="content"]')
        .fill(
          "This is an end-to-end test show blog post created by Playwright."
        );
      await page
        .locator('input[name="tags"]')
        .fill("e2e-test, playwright");

      await page.getByRole("button", { name: "SAVE POST" }).click();
      await page.waitForURL(
        new RegExp(`/dashboard/blog/${showSlug}/\\d+`)
      );
      await expect(page.locator("#banner-container")).toContainText(
        /created/i
      );
      await expect(page.getByText(postTitle)).toBeVisible();
    });

    test("created post appears in list", async ({ page }) => {
      await page.goto(`/dashboard/blog/${showSlug}`);
      await expect(
        page.locator("#blog-posts-table-body")
      ).toContainText(postTitle);
    });

    test("edit created post via dropdown", async ({ page }) => {
      await page.goto(`/dashboard/blog/${showSlug}`);
      const row = page.locator("#blog-posts-table-body tr", {
        hasText: postTitle,
      });
      await row.locator("select").selectOption("edit");
      await page.waitForURL(/\/edit/);

      // Verify title is pre-filled.
      await expect(page.locator('input[name="title"]')).toHaveValue(
        postTitle
      );

      // Submit without changes.
      await page.getByRole("button", { name: "SAVE CHANGES" }).click();
      await page.waitForURL(
        new RegExp(`/dashboard/blog/${showSlug}/\\d+`)
      );
      await expect(page.locator("#banner-container")).toContainText(
        /updated|Updated/i
      );
    });

    test("delete created post from list", async ({ page }) => {
      await page.goto(`/dashboard/blog/${showSlug}`);
      const row = page.locator("#blog-posts-table-body tr", {
        hasText: postTitle,
      });
      await expect(row).toBeVisible();

      // Show blog delete uses hx-confirm on a hidden button triggered
      // by the dropdown. The hx-confirm fires a native dialog.
      page.once("dialog", (d) => d.accept());
      await row.locator("select").selectOption("delete");

      // Delete swaps outerHTML of the row — the row should disappear.
      await expect(row).not.toBeVisible();
    });
  });
});
