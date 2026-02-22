import { test, expect } from "@playwright/test";
import { STAFF_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Events CRUD tests
//
// Covers list, create, detail, edit, feature toggle, and delete for the
// events dashboard. Uses STAFF_AUTH (minimum role for event management).
//
// Create and delete are grouped in a serial block so the created event
// can be cleaned up. Edit and feature tests use existing mock data.
// ---------------------------------------------------------------------------

test.use({ storageState: STAFF_AUTH });

test.describe("Events CRUD", () => {
  // -------------------------------------------------------------------------
  // List
  // -------------------------------------------------------------------------

  test("list page loads with events table", async ({ page }) => {
    await page.goto("/dashboard/events");
    await expect(page.locator("header h1")).toHaveText("Events");
    await expect(page.locator("#events-table-body")).toBeVisible();
    const rows = page.locator("#events-table-body tr");
    await expect(rows.first()).toBeVisible();
  });

  test("list page shows known mock event", async ({ page }) => {
    await page.goto("/dashboard/events");
    await expect(page.locator("#events-table-body")).toContainText(
      "Summer Block Party"
    );
  });

  // -------------------------------------------------------------------------
  // Detail (via table row click)
  // -------------------------------------------------------------------------

  test("clicking a row navigates to event detail", async ({ page }) => {
    await page.goto("/dashboard/events");
    const row = page.locator("#events-table-body tr", {
      hasText: "Summer Block Party",
    });
    // Click a non-action cell (the title cell).
    await row.locator("td").first().click();
    await page.waitForURL(/\/dashboard\/events\/\d+\//);
    await expect(page.getByText("Summer Block Party")).toBeVisible();
    await expect(page.getByText("KPBJ Community Plaza")).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Edit (existing mock event)
  // -------------------------------------------------------------------------

  test("edit form loads pre-filled and saves changes", async ({ page }) => {
    await page.goto("/dashboard/events");
    const row = page.locator("#events-table-body tr", {
      hasText: "Summer Block Party",
    });
    // Use actions dropdown to navigate to edit.
    await row.locator("select").selectOption("edit");
    await page.waitForURL(/\/edit/);

    // Verify form is pre-filled.
    await expect(page.locator('input[name="title"]')).toHaveValue(
      "Summer Block Party"
    );

    // Submit without changes — should still redirect with banner.
    await page.getByRole("button", { name: "UPDATE EVENT" }).click();
    await page.waitForURL(/\/dashboard\/events\/\d+\//);
    await expect(page.locator("#banner-container")).toContainText(/updated|Updated/i);
  });

  // -------------------------------------------------------------------------
  // Feature toggle (existing mock event)
  // -------------------------------------------------------------------------

  test("feature toggle updates row with banner", async ({ page }) => {
    await page.goto("/dashboard/events");
    const row = page.locator("#events-table-body tr", {
      hasText: "Summer Block Party",
    });
    // "Promote" or "Demote" — no confirm dialog for this action.
    const dropdown = row.locator("select");
    // Get current option text for the feature action.
    const featureOption = dropdown.locator('option[value="feature"]');
    await expect(featureOption).toBeAttached();
    await dropdown.selectOption("feature");
    // Should see a banner response.
    await expect(page.locator("#banner-container")).toContainText(
      /Promoted|Demoted/
    );
  });

  // -------------------------------------------------------------------------
  // Create → Delete (serial to clean up)
  // -------------------------------------------------------------------------

  test.describe.serial("create and delete event", () => {
    const eventTitle = `E2E Test Event ${Date.now()}`;

    test("create event via form", async ({ page }) => {
      await page.goto("/dashboard/events/new");
      await expect(page.getByText("NEW EVENT")).toBeVisible();

      // Fill required fields.
      await page.locator('input[name="title"]').fill(eventTitle);
      await page
        .locator('textarea[name="description"]')
        .fill("An end-to-end test event created by Playwright.");
      await page
        .locator('input[name="starts_at"]')
        .fill("2027-06-15T14:00");
      await page
        .locator('input[name="ends_at"]')
        .fill("2027-06-15T18:00");
      await page
        .locator('input[name="location_name"]')
        .fill("E2E Test Venue");
      await page
        .locator('input[name="location_address"]')
        .fill("123 Test Street, Sun Valley, CA 91352");

      await page.getByRole("button", { name: "CREATE EVENT" }).click();
      await page.waitForURL(/\/dashboard\/events\/\d+\//);
      await expect(page.locator("#banner-container")).toContainText(
        /created/i
      );
      await expect(page.getByText(eventTitle)).toBeVisible();
    });

    test("created event appears in list", async ({ page }) => {
      await page.goto("/dashboard/events");
      await expect(page.locator("#events-table-body")).toContainText(
        eventTitle
      );
    });

    test("delete created event", async ({ page }) => {
      await page.goto("/dashboard/events");
      const row = page.locator("#events-table-body tr", {
        hasText: eventTitle,
      });
      await expect(row).toBeVisible();

      // Accept the confirmation dialog before triggering delete.
      page.once("dialog", (d) => d.accept());
      await row.locator("select").selectOption("delete");

      // Delete redirects to events list with banner.
      await page.waitForURL(/\/dashboard\/events/);
      await expect(page.locator("#banner-container")).toContainText(
        /deleted/i
      );
      // Event should no longer be in the table.
      await expect(
        page.locator("#events-table-body").getByText(eventTitle)
      ).not.toBeVisible();
    });
  });
});
