import { test, expect } from "@playwright/test";
import { HOST_AUTH } from "../../playwright.config";
import path from "path";

// ---------------------------------------------------------------------------
// Ephemeral Uploads CRUD tests
//
// Covers list, validation, and create/delete for the ephemeral uploads
// dashboard. Uses HOST_AUTH (minimum role via requireHostNotSuspended).
//
// Ephemeral uploads are audio clips for automated nighttime/fallback playback.
// Uses staged audio upload — identical pattern to station IDs.
// station-ids-crud.spec.ts covers the client-side check of an empty form,
// which all upload forms share.
// ---------------------------------------------------------------------------

const AUDIO_FIXTURE = path.join(__dirname, "..", "..", "fixtures", "test-audio.mp3");

// The description must have at least 80 characters.
const DESCRIPTION =
  "This is an end-to-end test ephemeral upload with a description that exceeds the eighty character minimum validation requirement.";

test.use({ storageState: HOST_AUTH });

test.describe("Ephemeral Uploads CRUD", () => {
  test("list page shows the empty state and an Upload Ephemeral button", async ({ page }) => {
    await page.goto("/dashboard/ephemeral-uploads");
    await expect(page.getByText("No ephemeral uploads yet.")).toBeVisible();
    await expect(page.getByRole("link", { name: "Upload Ephemeral" })).toBeVisible();
  });

  test("submit without audio file shows server error", async ({ page }) => {
    await page.goto("/dashboard/ephemeral-uploads/new");
    await page.locator('input[name="title"]').fill("Missing Audio Test");
    await page.locator('textarea[name="description"]').fill(DESCRIPTION);
    // Bypass client-side validation by removing the required attributes.
    await page.locator('input[name="title"]').evaluate((el) => {
      el.removeAttribute("required");
    });
    await page.locator('textarea[name="description"]').evaluate((el) => {
      el.removeAttribute("required");
      el.removeAttribute("minlength");
    });
    await page.getByRole("button", { name: "UPLOAD" }).click();
    await expect(page.locator("#banner-container")).toContainText(/audio file is required/i);
  });

  // One test, so the delete step cleans up what the create step made.
  test("create and delete ephemeral upload", async ({ page }) => {
    const title = `E2E Ephemeral ${Date.now()}`;

    await test.step("create via upload form", async () => {
      await page.goto("/dashboard/ephemeral-uploads/new");
      await page.locator('input[name="title"]').fill(title);
      await page.locator('textarea[name="description"]').fill(DESCRIPTION);

      // The .fb-audio-uploaded element becomes visible once Alpine sets
      // uploadToken after a successful JSON response from the staged upload.
      await page.locator("#audio_file-input").setInputFiles(AUDIO_FIXTURE);
      await expect(page.locator(".fb-audio-uploaded")).toBeVisible({ timeout: 15_000 });
      await expect(page.locator(".fb-audio-uploaded-name")).toContainText("test-audio.mp3");

      await page.getByRole("button", { name: "UPLOAD" }).click();
      await page.waitForURL(/\/dashboard\/ephemeral-uploads/);
      await expect(page.locator("#banner-container")).toContainText(/uploaded/i);
    });

    await test.step("delete from the list", async () => {
      await page.goto("/dashboard/ephemeral-uploads");
      const row = page.locator("#ephemeral-uploads-table-body tr", { hasText: title });
      await expect(row).toBeVisible();

      // Accept the confirmation dialog before triggering delete.
      page.once("dialog", (d) => d.accept());
      await row.locator("select").selectOption("delete");

      // Delete is an in-place OOB swap (Pattern C) — no redirect.
      await expect(page.locator("#banner-container")).toContainText(/deleted/i);
      await expect(
        page.locator("#ephemeral-uploads-table-body").getByText(title)
      ).not.toBeVisible();
    });
  });
});
