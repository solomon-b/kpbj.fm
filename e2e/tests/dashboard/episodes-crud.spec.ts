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
// Uses staged audio file uploads — same pattern as station IDs and ephemeral.
// station-ids-crud.spec.ts covers the client-side check of an empty form,
// which all upload forms share.
//
// The show has a 2 hour slot, so the episode must run 1:57:00 to 1:58:00.
// `just e2e-fixtures` makes this file. It is too large for git.
// ---------------------------------------------------------------------------

const AUDIO_FIXTURE = path.join(__dirname, "..", "..", "fixtures", "episode-audio.mp3");
const SHOW_SLUG = "sunday-morning-jazz";
const LIST_URL = `/dashboard/episodes/${SHOW_SLUG}`;
const NEW_URL = `/dashboard/shows/${SHOW_SLUG}/episodes/new`;

// Shared across describe blocks — "Episodes Archive" depends on "Episodes CRUD"
// having created this episode first. Tests within a single file run sequentially
// in Playwright, so the HOST_AUTH block completes before the ADMIN_AUTH block.
const episodeDescription = `E2E Episode Test ${Date.now()}`;

// ---------------------------------------------------------------------------
// Host: list, form, create, verify
// ---------------------------------------------------------------------------

test.describe("Episodes CRUD", () => {
  test.use({ storageState: HOST_AUTH });

  test("list page shows the mock episodes and a New Episode button", async ({ page }) => {
    await page.goto(LIST_URL);
    // Mock data seeds 3 episodes for sunday-morning-jazz.
    const rows = page.locator("#episodes-table-body tr");
    await expect(rows.first()).toBeVisible();
    expect(await rows.count()).toBeGreaterThanOrEqual(3);
    await expect(page.getByRole("link", { name: "New Episode" })).toBeVisible();
  });

  test("submit without audio file shows server error", async ({ page }) => {
    await page.goto(NEW_URL);
    // Select the first available date.
    const select = page.locator('select[name="scheduled_date"]');
    const value = await select.locator("option:not([value=''])").first().getAttribute("value");
    await select.selectOption(value!);
    await page.locator('textarea[name="description"]').fill("Test description for missing audio validation.");
    // Bypass client-side validation.
    await page.locator('textarea[name="description"]').evaluate((el) => {
      el.removeAttribute("required");
    });
    await select.evaluate((el) => {
      el.removeAttribute("required");
    });
    await page.getByRole("button", { name: "SUBMIT" }).click();
    await expect(page.locator("#banner-container")).toContainText(/audio file is required/i);
  });

  test("create episode via upload form", async ({ page }) => {
    await page.goto(NEW_URL);

    // Select the last available scheduled date to avoid collision with
    // episode-schedule.spec.ts which picks the first date concurrently.
    const select = page.locator('select[name="scheduled_date"]');
    const options = select.locator("option:not([value=''])");
    const count = await options.count();
    expect(count).toBeGreaterThanOrEqual(1);
    await select.selectOption((await options.nth(count - 1).getAttribute("value"))!);

    await page.locator('textarea[name="description"]').fill(episodeDescription);
    await page.locator('input[name="tags"]').fill("e2e, playwright");

    // Upload audio file via staged upload, and wait for the async XHR.
    await page.locator("#audio_file-input").setInputFiles(AUDIO_FIXTURE);
    await expect(page.locator(".fb-audio-uploaded")).toBeVisible({ timeout: 15_000 });
    await expect(page.locator(".fb-audio-uploaded-name")).toContainText("episode-audio.mp3");

    // Submit. The server redirects to the episode detail page.
    await page.getByRole("button", { name: "SUBMIT" }).click();
    await page.waitForURL(/\/dashboard\/episodes\/sunday-morning-jazz\/\d+/);
    await expect(page.locator("#banner-container")).toContainText(/uploaded/i);

    await expect(page.getByText(episodeDescription)).toBeVisible();
    await expect(page.getByText("Preview Audio")).toBeVisible();
    await expect(page.getByText("#e2e")).toBeVisible();
    await expect(page.getByText("#playwright")).toBeVisible();
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
    await expect(page.locator("#banner-container")).toContainText(/archived/i);
  });
});
