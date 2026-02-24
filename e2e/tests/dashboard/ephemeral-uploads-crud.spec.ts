import { test, expect } from "@playwright/test";
import { HOST_AUTH } from "../../playwright.config";
import path from "path";

// ---------------------------------------------------------------------------
// Ephemeral Uploads CRUD tests
//
// Covers list, upload form, validation, and create/delete for the ephemeral
// uploads dashboard. Uses HOST_AUTH (minimum role via requireHostNotSuspended).
//
// Ephemeral uploads are audio clips for automated nighttime/fallback playback.
// Uses staged audio upload — identical pattern to station IDs.
// ---------------------------------------------------------------------------

const AUDIO_FIXTURE = path.join(__dirname, "..", "..", "fixtures", "test-audio.mp3");

test.use({ storageState: HOST_AUTH });

test.describe("Ephemeral Uploads CRUD", () => {
  // -------------------------------------------------------------------------
  // List
  // -------------------------------------------------------------------------

  test("list page loads with empty state", async ({ page }) => {
    await page.goto("/dashboard/ephemeral-uploads");
    await expect(
      page.getByText("No ephemeral uploads yet.")
    ).toBeVisible();
  });

  test("list page has Upload Ephemeral button", async ({ page }) => {
    await page.goto("/dashboard/ephemeral-uploads");
    await expect(
      page.getByRole("link", { name: "Upload Ephemeral" })
    ).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Upload form
  // -------------------------------------------------------------------------

  test("upload form loads with expected fields", async ({ page }) => {
    await page.goto("/dashboard/ephemeral-uploads/new");
    await expect(page.locator('input[name="title"]')).toBeVisible();
    await expect(page.locator('textarea[name="description"]')).toBeVisible();
    await expect(
      page.getByRole("button", { name: "CHOOSE AUDIO FILE" })
    ).toBeVisible();
    await expect(
      page.getByRole("button", { name: "UPLOAD" })
    ).toBeVisible();
  });

  test("upload form shows editorial guidelines", async ({ page }) => {
    await page.goto("/dashboard/ephemeral-uploads/new");
    await expect(
      page.getByText("Ephemeral clips are inferred as coming from the voice of the station")
    ).toBeVisible();
  });

  test("submit with empty title shows validation error", async ({ page }) => {
    await page.goto("/dashboard/ephemeral-uploads/new");
    await page.getByRole("button", { name: "UPLOAD" }).click();
    await expect(page.locator(".fb-error").first()).toBeVisible();
  });

  test("submit without audio file shows server error", async ({ page }) => {
    await page.goto("/dashboard/ephemeral-uploads/new");
    await page.locator('input[name="title"]').fill("Missing Audio Test");
    await page.locator('textarea[name="description"]').fill(
      "This is a test description that is long enough to pass the eighty character minimum validation requirement."
    );
    // Bypass client-side validation by removing the required attributes.
    await page.locator('input[name="title"]').evaluate((el) => {
      el.removeAttribute("required");
    });
    await page.locator('textarea[name="description"]').evaluate((el) => {
      el.removeAttribute("required");
      el.removeAttribute("minlength");
    });
    await page.getByRole("button", { name: "UPLOAD" }).click();
    await expect(page.locator("#banner-container")).toContainText(
      /audio file is required/i
    );
  });

  // -------------------------------------------------------------------------
  // Create → Verify → Delete (serial to clean up)
  // -------------------------------------------------------------------------

  test.describe.serial("create and delete ephemeral upload", () => {
    const ephemeralTitle = `E2E Ephemeral ${Date.now()}`;
    const ephemeralDescription =
      "This is an end-to-end test ephemeral upload with a description that exceeds the eighty character minimum validation requirement.";

    test("create ephemeral upload via upload form", async ({ page }) => {
      await page.goto("/dashboard/ephemeral-uploads/new");

      // Fill title.
      await page.locator('input[name="title"]').fill(ephemeralTitle);

      // Fill description (80 char minimum).
      await page.locator('textarea[name="description"]').fill(ephemeralDescription);

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
      await page.waitForURL(/\/dashboard\/ephemeral-uploads/);
      await expect(page.locator("#banner-container")).toContainText(
        /uploaded/i
      );
    });

    test("created ephemeral upload appears in list", async ({ page }) => {
      await page.goto("/dashboard/ephemeral-uploads");
      await expect(
        page.locator("#ephemeral-uploads-table-body")
      ).toContainText(ephemeralTitle);
    });

    test("delete created ephemeral upload", async ({ page }) => {
      await page.goto("/dashboard/ephemeral-uploads");
      const row = page.locator("#ephemeral-uploads-table-body tr", {
        hasText: ephemeralTitle,
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
        page.locator("#ephemeral-uploads-table-body").getByText(ephemeralTitle)
      ).not.toBeVisible();
    });
  });
});
