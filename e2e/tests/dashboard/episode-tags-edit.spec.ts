import { test, expect, type Page } from "@playwright/test";
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

async function submitAndWaitForList(page: Page) {
  await page.getByRole("button", { name: "SAVE CHANGES" }).click();
  await page.waitForURL((url) => !url.pathname.includes("/edit"));
  await expect(page.locator("#banner-container")).toContainText(/updated/i);
}

// Save the form as it is, open it again, and return the saved tags.
async function saveAndReload(page: Page, editUrl: string): Promise<string> {
  await submitAndWaitForList(page);
  await page.goto(editUrl);
  await page.waitForURL(/\/edit/);
  return await page.locator('input[name="tags"]').inputValue();
}

// ---------------------------------------------------------------------------
// Tag edit tests (HOST — host is assigned to sunday-morning-jazz)
//
// One test, because each step edits the tags that the step before saved.
// ---------------------------------------------------------------------------

test.describe("Episode Tag Editing", () => {
  test.use({ storageState: HOST_AUTH });

  test("tags persist through edits", async ({ page }) => {
    await page.goto(LIST_URL);
    const row = page.locator("#episodes-table-body tr", { hasText: MOCK_EPISODE_TEXT }).first();
    await expect(row).toBeVisible();
    await row.locator("select").selectOption("edit");
    await page.waitForURL(/\/dashboard\/episodes\/sunday-morning-jazz\/\d+\/edit/);
    const editUrl = page.url();
    const tagsInput = page.locator('input[name="tags"]');

    // Core regression check. Before the fix, the CTE snapshot bug caused
    // ON CONFLICT DO NOTHING to skip re-inserting assignments that were
    // deleted in the same statement, silently wiping all tags on every edit.
    await test.step("tags survive re-save without changes", async () => {
      await expect(tagsInput).toHaveValue(/classics/);
      expect(await saveAndReload(page, editUrl)).toContain("classics");
    });

    await test.step("tags can be changed to new values", async () => {
      await tagsInput.fill("test-tag-alpha, test-tag-beta");
      const saved = await saveAndReload(page, editUrl);
      expect(saved).toContain("test-tag-alpha");
      expect(saved).toContain("test-tag-beta");
      expect(saved).not.toContain("classics");
    });

    await test.step("changed tags survive a second re-save", async () => {
      const saved = await saveAndReload(page, editUrl);
      expect(saved).toContain("test-tag-alpha");
      expect(saved).toContain("test-tag-beta");
    });

    await test.step("tags can be cleared", async () => {
      await tagsInput.clear();
      expect(await saveAndReload(page, editUrl)).toBe("");
    });
  });
});
