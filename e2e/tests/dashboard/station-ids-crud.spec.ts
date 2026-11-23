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
// Uses staged audio file uploads: setInputFiles(), then a wait on the Alpine
// DOM signal for async XHR completion. The episode and ephemeral upload forms
// share the same form builder, so the client-side empty form check here
// covers them too.
// ---------------------------------------------------------------------------

const AUDIO_FIXTURE = path.join(__dirname, "..", "..", "fixtures", "test-audio.mp3");
const NEW_URL = "/dashboard/air-breaks/station-ids/new";

test.use({ storageState: HOST_AUTH });

test.describe("Station IDs CRUD", () => {
  test("list page shows the empty state and an Upload Station ID button", async ({ page }) => {
    await page.goto("/dashboard/air-breaks/station-ids");
    await expect(page.getByText("No station IDs uploaded yet.")).toBeVisible();
    await expect(page.getByRole("link", { name: "Upload Station ID" })).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Validation
  // -------------------------------------------------------------------------

  test("submit with empty title shows validation error", async ({ page }) => {
    await page.goto(NEW_URL);
    await page.getByRole("button", { name: "UPLOAD" }).click();
    await expect(page.getByText("Please check this field").first()).toBeVisible();
  });

  test("submit without audio file shows server error", async ({ page }) => {
    await page.goto(NEW_URL);
    await page.locator('input[name="title"]').fill("Missing Audio Test");
    // Bypass client-side validation by removing the required attribute.
    await page.locator('input[name="title"]').evaluate((el) => {
      el.removeAttribute("required");
    });
    await page.getByRole("button", { name: "UPLOAD" }).click();
    await expect(page.locator("#banner-container")).toContainText(/audio file is required/i);
  });

  test("server error with a file chosen keeps the form", async ({ page }) => {
    await page.goto(NEW_URL);

    // The client accepts "<>" as a title. The server strips angle brackets,
    // gets an empty title, and returns an OOB banner with HX-Reswap: none.
    await page.locator('input[name="title"]').fill("<>");

    // The file input keeps the chosen file after the staged upload.
    await page.locator("#audio_file-input").setInputFiles(AUDIO_FIXTURE);
    await expect(page.locator(".fb-audio-uploaded")).toBeVisible({ timeout: 15_000 });

    await page.getByRole("button", { name: "UPLOAD" }).click();

    await expect(page.locator("#banner-container")).toContainText(/title is required/i);
    await expect(page.locator('input[name="title"]')).toHaveValue("<>");
  });

  // -------------------------------------------------------------------------
  // Create → Delete (one test, so the delete step cleans up)
  // -------------------------------------------------------------------------

  test("create and delete station ID", async ({ page }) => {
    const title = `E2E Station ID ${Date.now()}`;

    await test.step("create via upload form", async () => {
      await page.goto(NEW_URL);
      await page.locator('input[name="title"]').fill(title);

      // The .fb-audio-uploaded element becomes visible once Alpine sets
      // uploadToken after a successful JSON response from the staged upload.
      await page.locator("#audio_file-input").setInputFiles(AUDIO_FIXTURE);
      await expect(page.locator(".fb-audio-uploaded")).toBeVisible({ timeout: 15_000 });
      await expect(page.locator(".fb-audio-uploaded-name")).toContainText("test-audio.mp3");

      await page.getByRole("button", { name: "UPLOAD" }).click();
      await page.waitForURL(/\/dashboard\/air-breaks\/station-ids/);
      await expect(page.locator("#banner-container")).toContainText(/uploaded/i);
    });

    await test.step("delete from the list", async () => {
      await page.goto("/dashboard/air-breaks/station-ids");
      const row = page.locator("#station-ids-table-body tr", { hasText: title });
      await expect(row).toBeVisible();

      // Accept the confirmation dialog before triggering delete.
      page.once("dialog", (d) => d.accept());
      await row.getByRole("button", { name: "Delete", exact: true }).click();

      // Delete is an in-place OOB swap (Pattern C) — no redirect.
      await expect(page.locator("#banner-container")).toContainText(/deleted/i);
      await expect(page.locator("#station-ids-table-body").getByText(title)).not.toBeVisible();
    });
  });
});
