import { test, expect } from "@playwright/test";
import { STAFF_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Station Blog CRUD tests
//
// Covers list, create, detail, edit, and delete for station-wide blog posts.
// Uses STAFF_AUTH (minimum role for station blog management).
//
// Create and delete are grouped in a serial block for cleanup.
// ---------------------------------------------------------------------------

test.use({ storageState: STAFF_AUTH });

test.describe("Station Blog CRUD", () => {
  // -------------------------------------------------------------------------
  // List
  // -------------------------------------------------------------------------

  test("list page loads with blog table", async ({ page }) => {
    await page.goto("/dashboard/station-blog");
    await expect(page.locator("header h1")).toHaveText("Station Blog");
    await expect(page.locator("#station-blog-table-body")).toBeVisible();
    const rows = page.locator("#station-blog-table-body tr");
    await expect(rows.first()).toBeVisible();
  });

  test("list page shows known mock post", async ({ page }) => {
    await page.goto("/dashboard/station-blog");
    await expect(page.locator("#station-blog-table-body")).toContainText(
      "KPBJ Wins 2025 Community Radio Excellence Award"
    );
  });

  // -------------------------------------------------------------------------
  // Detail (via table row click)
  // -------------------------------------------------------------------------

  test("clicking a row navigates to post detail", async ({ page }) => {
    await page.goto("/dashboard/station-blog");
    const row = page.locator("#station-blog-table-body tr", {
      hasText: "KPBJ Wins 2025 Community Radio Excellence Award",
    });
    await row.locator("td").first().click();
    await page.waitForURL(/\/dashboard\/station-blog\/\d+\//);
    await expect(
      page.getByText("KPBJ Wins 2025 Community Radio Excellence Award")
    ).toBeVisible();
    await expect(page.getByText("Staff Member", { exact: true })).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Edit (existing mock post)
  // -------------------------------------------------------------------------

  test("edit form loads pre-filled and saves", async ({ page }) => {
    await page.goto("/dashboard/station-blog");
    const row = page.locator("#station-blog-table-body tr", {
      hasText: "KPBJ Wins 2025 Community Radio Excellence Award",
    });
    await row.locator("select").selectOption("edit");
    await page.waitForURL(/\/edit/);

    // Verify title is pre-filled.
    await expect(page.locator('input[name="title"]')).toHaveValue(
      "KPBJ Wins 2025 Community Radio Excellence Award"
    );

    // Submit without changes.
    await page.getByRole("button", { name: "UPDATE POST" }).click();
    await page.waitForURL(/\/dashboard\/station-blog\/\d+\//);
    await expect(page.locator("#banner-container")).toContainText(
      /updated|Updated/i
    );
  });

  // -------------------------------------------------------------------------
  // Create → Delete (serial to clean up)
  // -------------------------------------------------------------------------

  test.describe.serial("create and delete station blog post", () => {
    const postTitle = `E2E Station Post ${Date.now()}`;

    test("create post via form", async ({ page }) => {
      await page.goto("/dashboard/station-blog/new");
      await expect(page.getByText("NEW BLOG POST")).toBeVisible();

      // Fill required fields.
      await page.locator('input[name="title"]').fill(postTitle);
      await page
        .locator('textarea[name="content"]')
        .fill(
          "This is an end-to-end test blog post created by Playwright for CRUD testing."
        );
      await page
        .locator('input[name="tags"]')
        .fill("e2e-test, playwright");

      await page.getByRole("button", { name: "SAVE POST" }).click();
      await page.waitForURL(/\/dashboard\/station-blog\/\d+\//);
      await expect(page.locator("#banner-container")).toContainText(
        /created/i
      );
      await expect(page.getByText(postTitle)).toBeVisible();
    });

    test("created post appears in list", async ({ page }) => {
      await page.goto("/dashboard/station-blog");
      await expect(
        page.locator("#station-blog-table-body")
      ).toContainText(postTitle);
    });

    test("delete created post", async ({ page }) => {
      await page.goto("/dashboard/station-blog");
      const row = page.locator("#station-blog-table-body tr", {
        hasText: postTitle,
      });
      await expect(row).toBeVisible();

      page.once("dialog", (d) => d.accept());
      await row.locator("select").selectOption("delete");

      await page.waitForURL(/\/dashboard\/station-blog/);
      await expect(page.locator("#banner-container")).toContainText(
        /deleted/i
      );
      await expect(
        page.locator("#station-blog-table-body").getByText(postTitle)
      ).not.toBeVisible();
    });
  });
});
