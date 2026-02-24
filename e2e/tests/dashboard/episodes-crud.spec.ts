import { test, expect } from "@playwright/test";
import { ADMIN_AUTH, HOST_AUTH } from "../../playwright.config";
import path from "path";

// ---------------------------------------------------------------------------
// Episode Upload CRUD tests
//
// Covers list, upload form, validation, create/verify/archive for episodes.
//
// Auth split:
// - HOST_AUTH for list, form, create, verify (host is assigned to the show)
// - ADMIN_AUTH for archive (only staff+ can archive; admin sees all shows)
//
// Staff can't see show-specific episode lists because fetchShowsForUser only
// returns all shows for admins — staff sees only their assigned shows.
//
// Uses staged audio file uploads — same pattern as station IDs and ephemeral.
// ---------------------------------------------------------------------------

const AUDIO_FIXTURE = path.join(__dirname, "..", "..", "fixtures", "test-audio.mp3");
const SHOW_SLUG = "sunday-morning-jazz";
const LIST_URL = `/dashboard/episodes/${SHOW_SLUG}`;
const NEW_URL = `/dashboard/shows/${SHOW_SLUG}/episodes/new`;

// Shared across describe blocks — "Episodes Archive" depends on "Episodes CRUD"
// having created this episode first. Tests within a single file run sequentially
// in Playwright, so the HOST_AUTH block completes before the ADMIN_AUTH block.
const episodeDescription = `E2E Episode Test ${Date.now()}`;
const episodeTags = "e2e, playwright";

// ---------------------------------------------------------------------------
// Host: list, form, create, verify
// ---------------------------------------------------------------------------

test.describe("Episodes CRUD", () => {
  test.use({ storageState: HOST_AUTH });

  // -------------------------------------------------------------------------
  // List
  // -------------------------------------------------------------------------

  test("list page loads with mock episodes", async ({ page }) => {
    await page.goto(LIST_URL);
    // Mock data seeds 3 episodes for sunday-morning-jazz.
    const rows = page.locator("#episodes-table-body tr");
    await expect(rows.first()).toBeVisible();
    expect(await rows.count()).toBeGreaterThanOrEqual(3);
  });

  test("list page has New Episode button", async ({ page }) => {
    await page.goto(LIST_URL);
    await expect(
      page.getByRole("link", { name: "New Episode" })
    ).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Upload form
  // -------------------------------------------------------------------------

  test("upload form loads with expected fields", async ({ page }) => {
    await page.goto(NEW_URL);
    await expect(page.locator('select[name="scheduled_date"]')).toBeVisible();
    await expect(page.locator('textarea[name="description"]')).toBeVisible();
    await expect(page.locator('input[name="tags"]')).toBeVisible();
    await expect(
      page.getByRole("button", { name: "CHOOSE AUDIO FILE" })
    ).toBeVisible();
    await expect(
      page.getByRole("button", { name: "SUBMIT" })
    ).toBeVisible();
  });

  test("scheduled date select has available options", async ({ page }) => {
    await page.goto(NEW_URL);
    const select = page.locator('select[name="scheduled_date"]');
    // Should have the placeholder plus at least one upcoming date.
    const options = select.locator("option");
    expect(await options.count()).toBeGreaterThanOrEqual(2);
  });

  test("submit with empty fields shows validation error", async ({ page }) => {
    await page.goto(NEW_URL);
    await page.getByRole("button", { name: "SUBMIT" }).click();
    await expect(page.locator(".fb-error").first()).toBeVisible();
  });

  test("submit without audio file shows server error", async ({ page }) => {
    await page.goto(NEW_URL);
    // Select the first available date.
    const select = page.locator('select[name="scheduled_date"]');
    const firstOption = select.locator("option:not([value=''])").first();
    const value = await firstOption.getAttribute("value");
    await select.selectOption(value!);
    // Fill description.
    await page.locator('textarea[name="description"]').fill("Test description for missing audio validation.");
    // Bypass client-side validation.
    await page.locator('textarea[name="description"]').evaluate((el) => {
      el.removeAttribute("required");
    });
    await page.locator('select[name="scheduled_date"]').evaluate((el) => {
      el.removeAttribute("required");
    });
    await page.getByRole("button", { name: "SUBMIT" }).click();
    await expect(page.locator("#banner-container")).toContainText(
      /audio file is required/i
    );
  });

  // -------------------------------------------------------------------------
  // Create → Verify Detail → Verify List (serial)
  // -------------------------------------------------------------------------

  test.describe.serial("create and verify episode", () => {
    test("create episode via upload form", async ({ page }) => {
      await page.goto(NEW_URL);

      // Select the first available scheduled date.
      const select = page.locator('select[name="scheduled_date"]');
      const firstOption = select.locator("option:not([value=''])").first();
      const value = await firstOption.getAttribute("value");
      await select.selectOption(value!);

      // Fill description.
      await page.locator('textarea[name="description"]').fill(episodeDescription);

      // Fill tags.
      await page.locator('input[name="tags"]').fill(episodeTags);

      // Upload audio file via staged upload.
      await page.locator("#audio_file-input").setInputFiles(AUDIO_FIXTURE);

      // Wait for the async XHR upload to complete.
      await expect(page.locator(".fb-audio-uploaded")).toBeVisible({
        timeout: 15_000,
      });

      // Verify the uploaded file name is shown.
      await expect(
        page.locator(".fb-audio-uploaded-name")
      ).toContainText("test-audio.mp3");

      // Submit the form.
      await page.getByRole("button", { name: "SUBMIT" }).click();
      // Redirects to the episode detail page.
      await page.waitForURL(/\/dashboard\/episodes\/sunday-morning-jazz\/\d+/);
      await expect(page.locator("#banner-container")).toContainText(
        /uploaded/i
      );
    });

    test("episode detail page shows correct data", async ({ page }) => {
      // Navigate to the episodes list, then click into the newly created episode.
      await page.goto(LIST_URL);
      const row = page.locator("#episodes-table-body tr", {
        hasText: episodeDescription,
      });
      await expect(row).toBeVisible();
      // Click the row to navigate to the detail page.
      await row.locator("td").first().click();
      await page.waitForURL(/\/dashboard\/episodes\/sunday-morning-jazz\/\d+/);

      // Verify description is shown.
      await expect(page.getByText(episodeDescription)).toBeVisible();

      // Verify audio player section is present.
      await expect(page.getByText("Preview Audio")).toBeVisible();

      // Verify tags are displayed.
      await expect(page.getByText("#e2e")).toBeVisible();
      await expect(page.getByText("#playwright")).toBeVisible();
    });

    test("created episode appears in list", async ({ page }) => {
      await page.goto(LIST_URL);
      await expect(
        page.locator("#episodes-table-body")
      ).toContainText(episodeDescription);
    });
  });
});

// ---------------------------------------------------------------------------
// Admin: archive (only staff+ can archive; admin sees all shows)
// ---------------------------------------------------------------------------

test.describe("Episodes Archive", () => {
  test.use({ storageState: ADMIN_AUTH });

  test("archive created episode", async ({ page }) => {
    await page.goto(LIST_URL);
    const row = page.locator("#episodes-table-body tr", {
      hasText: episodeDescription,
    });
    await expect(row).toBeVisible();

    // Accept the confirmation dialog before triggering archive.
    page.once("dialog", (d) => d.accept());
    await row.locator("select").selectOption("archive");

    // Archive is an in-place OOB swap — no redirect.
    await expect(page.locator("#banner-container")).toContainText(
      /archived/i
    );
  });
});
