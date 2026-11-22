import { test, expect, type Page } from "@playwright/test";
import { CROSS_BROWSER } from "../playwright.config";

// ---------------------------------------------------------------------------
// Events pages (/events, /events/:id/:slug)
// ---------------------------------------------------------------------------
// Mock data has 12 events (6 past, 6 upcoming). Events are ordered by
// starts_at DESC. Cards show poster image, title, and date. Detail page
// adds location and description.

const RECORD_FAIR = "Spring Record Fair & Swap Meet";

// The event id comes from the database, so the test opens the event from
// the list.
async function openEvent(page: Page, title: string, slug: RegExp) {
  await page.goto("/events");
  await page.getByText(title).click();
  await page.waitForURL(slug);
}

test.describe("Events list", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/events");
  });

  test("displays the event cards", async ({ page }) => {
    await expect(page.locator("h1", { hasText: "EVENTS" })).toBeAttached();
    await expect(page.locator("article")).toHaveCount(12);
    await expect(page.locator("article h2")).toHaveCount(12);
    await expect(page.getByText(RECORD_FAIR)).toBeVisible();

    // Card posters have "<title> poster" alt text.
    await expect(page.locator(`article img[alt="${RECORD_FAIR} poster"]`)).toBeVisible();

    // Dates render as "Saturday, March 14, 2026" and times as
    // "H:MM AM - H:MM PM".
    const firstCard = page.locator("article").first();
    await expect(firstCard.getByText(/\w+, \w+ \d{1,2}, \d{4}/)).toBeVisible();
    await expect(
      firstCard.getByText(/\d{1,2}:\d{2} [AP]M\s*-\s*\d{1,2}:\d{2} [AP]M/)
    ).toBeVisible();
  });

  test("the title and the poster link to the detail page", async ({ page }) => {
    await page.locator("article h2 a").first().click();
    await expect(page).toHaveURL(/\/events\/\d+\/.+/);

    // The poster image is wrapped in its own <a> link to the detail page.
    await page.goto("/events");
    await page.locator("article a:has(img, [class*='aspect-'])").first().click();
    await expect(page).toHaveURL(/\/events\/\d+\/.+/);
  });
});

// ---------------------------------------------------------------------------
// Event detail page
// ---------------------------------------------------------------------------

test.describe("Event detail", () => {
  test("displays the event and no photo gallery", async ({ page }) => {
    await openEvent(page, RECORD_FAIR, /\/events\/\d+\/spring-record-fair/);

    await expect(page.locator("h1", { hasText: RECORD_FAIR })).toBeVisible();
    await expect(page.locator(`img[alt="${RECORD_FAIR} poster"]`)).toBeVisible();

    // March 14, 2026 is a Saturday, 10:00 AM - 4:00 PM Pacific.
    await expect(page.getByText("DATE & TIME")).toBeVisible();
    await expect(page.getByText("Saturday, March 14, 2026")).toBeVisible();
    await expect(page.getByText(/10:00 AM\s*-\s*4:00 PM/)).toBeVisible();

    await expect(page.getByText("LOCATION")).toBeVisible();
    await expect(page.getByText("KPBJ Studio Parking Lot")).toBeVisible();
    await expect(page.getByText("123 Radio Street, Sun Valley, CA 91352")).toBeVisible();
    await expect(page.getByText(/Dig through crates of vinyl/)).toBeVisible();

    // This event has no photos.
    await expect(page.getByRole("heading", { name: "PHOTOS" })).toHaveCount(0);
    await expect(page.locator(".eg-gallery-grid")).toHaveCount(0);
  });
});

// ---------------------------------------------------------------------------
// Event photo gallery (public)
// ---------------------------------------------------------------------------
// Mock data attaches 3 photos to "Summer Block Party". Other events have none.

test.describe("Event photo gallery", () => {
  test.beforeEach(async ({ page }) => {
    await openEvent(page, "Summer Block Party", /\/events\/\d+\/summer-block-party/);
  });

  test("shows the PHOTOS gallery grid with captions", async ({ page }) => {
    await expect(page.getByRole("heading", { name: "PHOTOS" })).toBeVisible();
    await expect(page.locator(".eg-gallery-item")).toHaveCount(3);

    // Scope to the thumbnail caption. The (hidden) lightbox caption holds
    // the same text.
    await expect(
      page.locator(".eg-gallery-caption", { hasText: "The crowd at the plaza" })
    ).toBeVisible();
  });

  test("lightbox opens, moves to the next photo, and closes on Escape", { tag: CROSS_BROWSER }, async ({ page }) => {
    const lightbox = page.locator(".eg-lightbox");
    const caption = page.locator(".eg-lightbox-caption");

    await expect(lightbox).toBeHidden();
    await page.locator(".eg-gallery-item").first().click();
    await expect(lightbox).toBeVisible();

    // The first photo (sort_order 0) opens first.
    await expect(caption).toHaveText("The crowd at the plaza");

    await page.locator(".eg-lightbox-nav--next").click();
    await expect(caption).toHaveText("Live band on the main stage");

    await page.keyboard.press("Escape");
    await expect(lightbox).toBeHidden();
  });
});
