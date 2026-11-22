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

// Fill the new event form and submit it. The server redirects to the
// dashboard detail page of the event.
async function createEvent(page: Page, title: string, date: string, venue: string) {
  await page.goto("/dashboard/events/new");
  await expect(page.getByText("NEW EVENT")).toBeVisible();
  await page.locator('input[name="title"]').fill(title);
  await page
    .locator('textarea[name="description"]')
    .fill("An end-to-end test event created by Playwright.");
  await page.locator('input[name="starts_at"]').fill(`${date}T14:00`);
  await page.locator('input[name="ends_at"]').fill(`${date}T18:00`);
  await page.locator('input[name="location_name"]').fill(venue);
  await page
    .locator('input[name="location_address"]')
    .fill("123 Test Street, Sun Valley, CA 91352");
  await page.getByRole("button", { name: "CREATE EVENT" }).click();
  await page.waitForURL(/\/dashboard\/events\/\d+\//);
}

// Delete an event from the list. Delete redirects to the list with a banner.
async function deleteEvent(page: Page, title: string) {
  await page.goto("/dashboard/events");
  const row = page.locator("#events-table-body tr", { hasText: title });
  await expect(row).toBeVisible();
  page.once("dialog", (d) => d.accept());
  await row.locator("select").selectOption("delete");
  await page.waitForURL(/\/dashboard\/events/);
}

// Save the edit form and wait for the dashboard detail page.
async function saveEvent(page: Page) {
  await page.getByRole("button", { name: "UPDATE EVENT" }).click();
  await page.waitForURL(/\/dashboard\/events\/\d+\//);
  await expect(page.locator("#banner-container")).toContainText(/updated/i);
}

// ---------------------------------------------------------------------------
// Events CRUD tests
//
// Covers list, create, detail, edit, feature toggle, and delete for the
// events dashboard. Uses STAFF_AUTH (minimum role for event management).
//
// Edit and feature tests use existing mock data.
// ---------------------------------------------------------------------------

test.use({ storageState: STAFF_AUTH });

test.describe("Events CRUD", () => {
  function blockPartyRow(page: Page) {
    return page.locator("#events-table-body tr", { hasText: "Summer Block Party" });
  }

  test("list shows the mock events and a row opens the detail page", async ({ page }) => {
    await page.goto("/dashboard/events");
    await expect(blockPartyRow(page)).toBeVisible();

    // Click a non-action cell (the title cell).
    await blockPartyRow(page).locator("td").first().click();
    await page.waitForURL(/\/dashboard\/events\/\d+\//);
    await expect(page.getByText("Summer Block Party")).toBeVisible();
    await expect(page.getByText("KPBJ Community Plaza")).toBeVisible();
  });

  test("edit form loads pre-filled and saves changes", async ({ page }) => {
    await page.goto("/dashboard/events");
    await blockPartyRow(page).locator("select").selectOption("edit");
    await page.waitForURL(/\/edit/);
    await expect(page.locator('input[name="title"]')).toHaveValue("Summer Block Party");

    // Submit without changes. It still redirects with a banner.
    await saveEvent(page);
  });

  test("feature toggle updates row with banner", async ({ page }) => {
    await page.goto("/dashboard/events");
    // "Promote" or "Demote". No confirm dialog for this action.
    const dropdown = blockPartyRow(page).locator("select");
    await expect(dropdown.locator('option[value="feature"]')).toBeAttached();
    await dropdown.selectOption("feature");
    await expect(page.locator("#banner-container")).toContainText(/Promoted|Demoted/);
  });

  // One test, so the delete step cleans up what the create step made.
  test("create and delete event", async ({ page }) => {
    const title = `E2E Test Event ${Date.now()}`;

    await test.step("create via form", async () => {
      await createEvent(page, title, "2027-06-15", "E2E Test Venue");
      await expect(page.locator("#banner-container")).toContainText(/created/i);
      await expect(page.getByText(title)).toBeVisible();
    });

    await test.step("delete from the list", async () => {
      await deleteEvent(page, title);
      await expect(page.locator("#banner-container")).toContainText(/deleted/i);
      await expect(page.locator("#events-table-body").getByText(title)).not.toBeVisible();
    });
  });
});

// ---------------------------------------------------------------------------
// Event photo gallery editor
//
// One test: it creates its own event, exercises upload / caption / reorder /
// delete through the edit form, verifies against the public page, then cleans
// up the event.
// ---------------------------------------------------------------------------

test.describe("Event photo gallery editor", () => {
  test("upload, caption, reorder, and delete photos", async ({ page }) => {
    const title = `E2E Gallery Event ${Date.now()}`;
    await createEvent(page, title, "2027-07-15", "Gallery Venue");

    const m = page.url().match(/\/dashboard\/events\/(\d+)\/([^/?]+)/);
    expect(m).not.toBeNull();
    const [, id, slug] = m!;
    const editUrl = `/dashboard/events/${id}/${slug}/edit`;
    const publicUrl = `/events/${id}/${slug}`;
    const cards = page.locator(".fb-images-card");
    const captionInputs = page.getByPlaceholder("Caption (optional)");

    await test.step("upload a photo with a caption and save", async () => {
      await page.goto(editUrl);
      await page.locator("#event-gallery-add-input").setInputFiles(IMAGE_FIXTURE);
      // Alpine renders a preview card for the new photo.
      await expect(cards).toHaveCount(1);
      await captionInputs.fill("Opening night");
      await saveEvent(page);
    });

    await test.step("photo persists and appears on the public page", async () => {
      await page.goto(editUrl);
      await expect(cards).toHaveCount(1);
      await expect(captionInputs.first()).toHaveValue("Opening night");

      await page.goto(publicUrl);
      await expect(page.getByRole("heading", { name: "PHOTOS" })).toBeVisible();
      await expect(page.locator(".eg-gallery-item")).toHaveCount(1);
      // Scope to the thumbnail caption; the lightbox caption holds the same text.
      await expect(
        page.locator(".eg-gallery-caption", { hasText: "Opening night" })
      ).toBeVisible();
    });

    await test.step("reordering photos persists", async () => {
      await page.goto(editUrl);
      // Add a second photo and caption it distinctly.
      await page.locator("#event-gallery-add-input").setInputFiles(IMAGE_FIXTURE);
      await expect(cards).toHaveCount(2);
      await captionInputs.nth(1).fill("After party");

      // Drag the second card in front of the first.
      await dragCard(page, 1, 0);
      await saveEvent(page);
      // Let the redirected dashboard page settle before navigating away, or the
      // in-flight requests abort the next goto (net::ERR_ABORTED).
      await page.waitForLoadState("networkidle");

      // "After party" is now sort_order 0, so it renders first on the public page.
      await page.goto(publicUrl);
      const captions = page.locator(".eg-gallery-caption");
      await expect(captions).toHaveCount(2);
      await expect(captions.first()).toHaveText("After party");
    });

    await test.step("deleting a photo removes it", async () => {
      await page.goto(editUrl);
      await expect(cards).toHaveCount(2);
      await page.locator(".fb-images-remove").first().click();
      await expect(cards).toHaveCount(1);
      await saveEvent(page);
      await page.waitForLoadState("networkidle");

      await page.goto(publicUrl);
      await expect(page.locator(".eg-gallery-item")).toHaveCount(1);
    });

    await test.step("cleanup: delete the event", async () => {
      await deleteEvent(page, title);
    });
  });
});
