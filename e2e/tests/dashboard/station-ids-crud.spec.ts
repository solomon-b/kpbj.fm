import { test, expect } from "@playwright/test";
import { HOST_AUTH } from "../../playwright.config";
import path from "path";

// ---------------------------------------------------------------------------
// Station IDs CRUD tests
//
// Covers list, upload form, validation, and create/delete for the station IDs
// dashboard. Uses HOST_AUTH (minimum role for station ID management via
// requireHostNotSuspended).
//
// This is the first test with staged audio file uploads — uses
// setInputFiles() and waits on Alpine DOM signals for async XHR completion.
// ---------------------------------------------------------------------------

const AUDIO_FIXTURE = path.join(__dirname, "..", "..", "fixtures", "test-audio.mp3");

test.use({ storageState: HOST_AUTH });

test.describe("Station IDs CRUD", () => {
  // -------------------------------------------------------------------------
  // List
  // -------------------------------------------------------------------------

  test("list page loads with empty state", async ({ page }) => {
    await page.goto("/dashboard/station-ids");
    await expect(page.locator("header h1")).toHaveText("Station IDs");
    await expect(
      page.getByText("No station IDs uploaded yet.")
    ).toBeVisible();
  });

  test("list page has Upload Station ID button", async ({ page }) => {
    await page.goto("/dashboard/station-ids");
    await expect(
      page.getByRole("link", { name: "Upload Station ID" })
    ).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Upload form
  // -------------------------------------------------------------------------

  test("upload form loads with expected fields", async ({ page }) => {
    await page.goto("/dashboard/station-ids/new");
    await expect(page.locator('input[name="title"]')).toBeVisible();
    await expect(
      page.getByRole("button", { name: "CHOOSE AUDIO FILE" })
    ).toBeVisible();
    await expect(
      page.getByRole("button", { name: "UPLOAD" })
    ).toBeVisible();
  });

  test("submit with empty title shows validation error", async ({ page }) => {
    await page.goto("/dashboard/station-ids/new");
    await page.getByRole("button", { name: "UPLOAD" }).click();
    await expect(page.locator(".fb-error").first()).toBeVisible();
  });

  test("submit without audio file shows server error", async ({ page }) => {
    await page.goto("/dashboard/station-ids/new");
    await page.locator('input[name="title"]').fill("Missing Audio Test");
    // Bypass client-side validation by removing the required attribute.
    await page.locator('input[name="title"]').evaluate((el) => {
      el.removeAttribute("required");
    });
    await page.getByRole("button", { name: "UPLOAD" }).click();
    await expect(page.locator("#banner-container")).toContainText(
      /audio file is required/i
    );
  });

  // -------------------------------------------------------------------------
  // Create → Delete (serial to clean up)
  // -------------------------------------------------------------------------

  test.describe.serial("create and delete station ID", () => {
    const stationIdTitle = `E2E Station ID ${Date.now()}`;

    test("create station ID via upload form", async ({ page }) => {
      await page.goto("/dashboard/station-ids/new");

      // Fill title.
      await page.locator('input[name="title"]').fill(stationIdTitle);

      // Upload audio file via staged upload.
      await page.locator("#audio_file-input").setInputFiles(AUDIO_FIXTURE);

      // Wait for the async XHR upload to complete — the .fb-audio-uploaded
      // element becomes visible once Alpine sets uploadToken after a
      // successful JSON response.
      await expect(page.locator(".fb-audio-uploaded")).toBeVisible({
        timeout: 15_000,
      });

      // Verify the uploaded file name is shown.
      await expect(
        page.locator(".fb-audio-uploaded-name")
      ).toContainText("test-audio.mp3");

      // Submit the form.
      await page.getByRole("button", { name: "UPLOAD" }).click();
      await page.waitForURL(/\/dashboard\/station-ids/);
      await expect(page.locator("#banner-container")).toContainText(
        /uploaded/i
      );
    });

    test("created station ID appears in list", async ({ page }) => {
      await page.goto("/dashboard/station-ids");
      await expect(
        page.locator("#station-ids-table-body")
      ).toContainText(stationIdTitle);
    });

    test("delete created station ID", async ({ page }) => {
      await page.goto("/dashboard/station-ids");
      const row = page.locator("#station-ids-table-body tr", {
        hasText: stationIdTitle,
      });
      await expect(row).toBeVisible();

      // Accept the confirmation dialog before triggering delete.
      page.once("dialog", (d) => d.accept());
      await row.locator("select").selectOption("delete");

      // Delete is an in-place OOB swap (Pattern C) — no redirect.
      await expect(page.locator("#banner-container")).toContainText(
        /deleted/i
      );
      // Row should be removed from the table.
      await expect(
        page.locator("#station-ids-table-body").getByText(stationIdTitle)
      ).not.toBeVisible();
    });
  });
});
