import { test, expect, Page } from "@playwright/test";
import path from "path";
import { STAFF_AUTH } from "../../playwright.config";

const IMAGE_FIXTURE = path.join(
  __dirname,
  "..",
  "..",
  "fixtures",
  "test-image.png"
);

// Reorder gallery cards via synthetic HTML5 drag events. Playwright's dragTo
// uses mouse events, which do not trigger the dragstart/dragover/drop handlers
// the editor listens on.
async function dragCard(page: Page, fromIdx: number, toIdx: number) {
  await page.evaluate(
    ([from, to]) => {
      const cards = document.querySelectorAll(".fb-images-card");
      const dt = new DataTransfer();
      const fire = (el: Element, type: string) =>
        el.dispatchEvent(
          new DragEvent(type, {
            bubbles: true,
            cancelable: true,
            dataTransfer: dt,
          })
        );
      fire(cards[from], "dragstart");
      fire(cards[to], "dragover");
      fire(cards[to], "drop");
      fire(cards[from], "dragend");
    },
    [fromIdx, toIdx]
  );
}

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

// ---------------------------------------------------------------------------
// Event photo gallery editor
//
// Serial block: creates its own event, exercises upload / caption / reorder /
// delete through the edit form, verifies against the public page, then cleans
// up the event.
// ---------------------------------------------------------------------------

test.describe.serial("Event photo gallery editor", () => {
  const title = `E2E Gallery Event ${Date.now()}`;
  let editUrl = "";
  let publicUrl = "";

  test("create an event to attach photos to", async ({ page }) => {
    await page.goto("/dashboard/events/new");
    await page.locator('input[name="title"]').fill(title);
    await page
      .locator('textarea[name="description"]')
      .fill("Event created by the gallery e2e test.");
    await page.locator('input[name="starts_at"]').fill("2027-07-15T14:00");
    await page.locator('input[name="ends_at"]').fill("2027-07-15T18:00");
    await page.locator('input[name="location_name"]').fill("Gallery Venue");
    await page
      .locator('input[name="location_address"]')
      .fill("1 Gallery Way, Sun Valley, CA 91352");
    await page.getByRole("button", { name: "CREATE EVENT" }).click();
    await page.waitForURL(/\/dashboard\/events\/\d+\//);

    const m = page.url().match(/\/dashboard\/events\/(\d+)\/([^/?]+)/);
    expect(m).not.toBeNull();
    const [, id, slug] = m!;
    editUrl = `/dashboard/events/${id}/${slug}/edit`;
    publicUrl = `/events/${id}/${slug}`;
  });

  test("upload a photo with a caption and save", async ({ page }) => {
    await page.goto(editUrl);
    await page.locator("#event-gallery-add-input").setInputFiles(IMAGE_FIXTURE);
    // Alpine renders a preview card for the new photo.
    await expect(page.locator(".fb-images-card")).toHaveCount(1);
    await page.getByPlaceholder("Caption (optional)").fill("Opening night");

    await page.getByRole("button", { name: "UPDATE EVENT" }).click();
    await page.waitForURL(/\/dashboard\/events\/\d+\//);
    await expect(page.locator("#banner-container")).toContainText(/updated/i);
  });

  test("uploaded photo persists on reload", async ({ page }) => {
    await page.goto(editUrl);
    await expect(page.locator(".fb-images-card")).toHaveCount(1);
    await expect(
      page.getByPlaceholder("Caption (optional)").first()
    ).toHaveValue("Opening night");
  });

  test("photo appears on the public event page", async ({ page }) => {
    await page.goto(publicUrl);
    await expect(page.getByRole("heading", { name: "PHOTOS" })).toBeVisible();
    await expect(page.locator(".eg-gallery-item")).toHaveCount(1);
    await expect(page.getByText("Opening night")).toBeVisible();
  });

  test("reordering photos persists", async ({ page }) => {
    await page.goto(editUrl);
    // Add a second photo and caption it distinctly.
    await page.locator("#event-gallery-add-input").setInputFiles(IMAGE_FIXTURE);
    await expect(page.locator(".fb-images-card")).toHaveCount(2);
    await page
      .getByPlaceholder("Caption (optional)")
      .nth(1)
      .fill("After party");

    // Drag the second card in front of the first.
    await dragCard(page, 1, 0);

    await page.getByRole("button", { name: "UPDATE EVENT" }).click();
    await page.waitForURL(/\/dashboard\/events\/\d+\//);

    // "After party" is now sort_order 0, so it renders first on the public page.
    await page.goto(publicUrl);
    const captions = page.locator(".eg-gallery-caption");
    await expect(captions).toHaveCount(2);
    await expect(captions.first()).toHaveText("After party");
  });

  test("deleting a photo removes it", async ({ page }) => {
    await page.goto(editUrl);
    await expect(page.locator(".fb-images-card")).toHaveCount(2);
    // Remove the first card.
    await page.locator(".fb-images-remove").first().click();
    await expect(page.locator(".fb-images-card")).toHaveCount(1);

    await page.getByRole("button", { name: "UPDATE EVENT" }).click();
    await page.waitForURL(/\/dashboard\/events\/\d+\//);

    await page.goto(publicUrl);
    await expect(page.locator(".eg-gallery-item")).toHaveCount(1);
  });

  test("cleanup: delete the event", async ({ page }) => {
    await page.goto("/dashboard/events");
    const row = page.locator("#events-table-body tr", { hasText: title });
    page.once("dialog", (d) => d.accept());
    await row.locator("select").selectOption("delete");
    await page.waitForURL(/\/dashboard\/events/);
  });
});
