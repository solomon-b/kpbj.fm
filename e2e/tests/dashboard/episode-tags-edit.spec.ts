import { test, expect } from "@playwright/test";
import { HOST_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Episode Tag Edit tests
//
// Regression tests for the CTE snapshot bug where editing an episode with
// existing tags would silently delete all tags. The root cause was that
// PostgreSQL CTE statements share a snapshot — a DELETE and INSERT on the
// same table within one CTE can't see each other's effects.
//
// Uses mock episodes from sunday-morning-jazz which are seeded with the
// "classics" tag via mock-data/18_episode_tag_assignments.sql.
// ---------------------------------------------------------------------------

const SHOW_SLUG = "sunday-morning-jazz";
const LIST_URL = `/dashboard/episodes/${SHOW_SLUG}`;

// Mock episodes have descriptions like "A great episode of Sunday Morning Jazz from ..."
const MOCK_EPISODE_TEXT = "A great episode of";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

async function navigateToEditByText(
  page: import("@playwright/test").Page,
  text: string,
) {
  await page.goto(LIST_URL);
  const row = page.locator("#episodes-table-body tr", { hasText: text }).first();
  await expect(row).toBeVisible();
  await row.locator("select").selectOption("edit");
  await page.waitForURL(
    /\/dashboard\/episodes\/sunday-morning-jazz\/\d+\/edit/,
  );
}

async function submitAndWaitForList(
  page: import("@playwright/test").Page,
) {
  await page.getByRole("button", { name: "SAVE CHANGES" }).click();
  await page.waitForURL((url) => !url.pathname.includes("/edit"));
  await expect(page.locator("#banner-container")).toContainText(/updated/i);
}

// ---------------------------------------------------------------------------
// Tag edit tests (HOST — host is assigned to sunday-morning-jazz)
// ---------------------------------------------------------------------------

test.describe("Episode Tag Editing", () => {
  test.use({ storageState: HOST_AUTH });

  test("edit form loads with existing tags pre-populated", async ({
    page,
  }) => {
    await navigateToEditByText(page, MOCK_EPISODE_TEXT);
    const tagsInput = page.locator('input[name="tags"]');
    await expect(tagsInput).toBeVisible();
    // Mock data assigns "classics" tag to sunday-morning-jazz episodes.
    await expect(tagsInput).toHaveValue(/classics/);
  });

  // -------------------------------------------------------------------------
  // Core regression test: tags survive a re-save without changes.
  //
  // Before the fix, the CTE snapshot bug caused ON CONFLICT DO NOTHING to
  // skip re-inserting assignments that were deleted in the same statement,
  // silently wiping all tags on every edit.
  // -------------------------------------------------------------------------

  test.describe.serial("tags persist through edits", () => {
    // Track which edit URL we're working with so all serial tests use
    // the same episode.
    let editUrl: string;

    test("tags survive re-save without changes", async ({ page }) => {
      // Navigate to edit form for a mock episode.
      await navigateToEditByText(page, MOCK_EPISODE_TEXT);
      editUrl = page.url();

      // Confirm tags are present before saving.
      const tagsInput = page.locator('input[name="tags"]');
      const originalTags = await tagsInput.inputValue();
      expect(originalTags).toContain("classics");

      // Submit without changing anything.
      await submitAndWaitForList(page);

      // Navigate back to the same edit form.
      await page.goto(editUrl);
      await page.waitForURL(/\/edit/);

      // Tags must still be present — this is the core regression check.
      await expect(page.locator('input[name="tags"]')).toHaveValue(
        /classics/,
      );
    });

    test("tags can be changed to new values", async ({ page }) => {
      await page.goto(editUrl);
      await page.waitForURL(/\/edit/);

      // Replace tags with new values.
      const tagsInput = page.locator('input[name="tags"]');
      await tagsInput.clear();
      await tagsInput.fill("test-tag-alpha, test-tag-beta");

      await submitAndWaitForList(page);

      // Verify new tags were saved.
      await page.goto(editUrl);
      await page.waitForURL(/\/edit/);
      const savedTags = await page.locator('input[name="tags"]').inputValue();
      expect(savedTags).toContain("test-tag-alpha");
      expect(savedTags).toContain("test-tag-beta");
      expect(savedTags).not.toContain("classics");
    });

    test("changed tags survive a second re-save", async ({ page }) => {
      await page.goto(editUrl);
      await page.waitForURL(/\/edit/);

      // Confirm previous edit persisted.
      const tagsInput = page.locator('input[name="tags"]');
      const currentTags = await tagsInput.inputValue();
      expect(currentTags).toContain("test-tag-alpha");
      expect(currentTags).toContain("test-tag-beta");

      // Re-save without changes.
      await submitAndWaitForList(page);

      // Verify tags survived.
      await page.goto(editUrl);
      await page.waitForURL(/\/edit/);
      await expect(page.locator('input[name="tags"]')).toHaveValue(
        /test-tag-alpha/,
      );
      await expect(page.locator('input[name="tags"]')).toHaveValue(
        /test-tag-beta/,
      );
    });

    test("tags can be cleared", async ({ page }) => {
      await page.goto(editUrl);
      await page.waitForURL(/\/edit/);

      // Clear all tags.
      const tagsInput = page.locator('input[name="tags"]');
      await tagsInput.clear();

      await submitAndWaitForList(page);

      // Verify tags are gone.
      await page.goto(editUrl);
      await page.waitForURL(/\/edit/);
      await expect(page.locator('input[name="tags"]')).toHaveValue("");
    });
  });
});
