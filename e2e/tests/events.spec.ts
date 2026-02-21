import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Events pages (/events, /events/:id/:slug)
// ---------------------------------------------------------------------------
// Mock data has 12 events (6 past, 6 upcoming). Events are ordered by
// starts_at DESC. Cards show poster image, title, and date. Detail page
// adds location and description.

test.describe("Events list", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/events");
  });

  // -------------------------------------------------------------------------
  // Page structure
  // -------------------------------------------------------------------------

  test("displays the EVENTS heading", async ({ page }) => {
    await expect(page.locator("h1", { hasText: "EVENTS" })).toBeAttached();
  });

  test("displays event cards", async ({ page }) => {
    const cards = page.locator("article");
    const count = await cards.count();
    // Mock data has 12 published events.
    expect(count).toBe(12);
  });

  // -------------------------------------------------------------------------
  // Event cards
  // -------------------------------------------------------------------------

  test("event cards have titles", async ({ page }) => {
    // Summary cards use <h2> for titles.
    const titles = page.locator("article h2");
    const count = await titles.count();
    expect(count).toBe(12);
  });

  test("event cards have poster images or placeholders", async ({ page }) => {
    const firstCard = page.locator("article").first();
    const hasImage = (await firstCard.locator("img").count()) > 0;
    const hasPlaceholder =
      (await firstCard.getByText("[NO POSTER]").count()) > 0;
    expect(hasImage || hasPlaceholder).toBe(true);
  });

  test("event poster images have alt text", async ({ page }) => {
    const images = page.locator("article img");
    const imgCount = await images.count();
    if (imgCount > 0) {
      const alt = await images.first().getAttribute("alt");
      expect(alt).toMatch(/.+ poster$/);
    }
  });

  test("event cards show dates", async ({ page }) => {
    // Dates are rendered as "Day, Month DD, YYYY" (e.g., "Saturday, March 14, 2026").
    const firstCard = page.locator("article").first();
    await expect(
      firstCard.getByText(/\w+, \w+ \d{1,2}, \d{4}/)
    ).toBeVisible();
  });

  test("event cards show time ranges", async ({ page }) => {
    // Times are rendered as "H:MM AM - H:MM PM".
    const firstCard = page.locator("article").first();
    await expect(
      firstCard.getByText(/\d{1,2}:\d{2} [AP]M\s*-\s*\d{1,2}:\d{2} [AP]M/)
    ).toBeVisible();
  });

  test("event titles link to detail pages", async ({ page }) => {
    const firstTitle = page.locator("article h2 a").first();
    const href = await firstTitle.getAttribute("href");
    expect(href).toMatch(/\/events\/\d+\/.+/);
  });

  test("clicking an event title navigates to its detail page", async ({ page }) => {
    const firstTitle = page.locator("article h2 a").first();
    await firstTitle.click();
    await expect(page).toHaveURL(/\/events\/\d+\/.+/);
  });

  test("clicking an event poster navigates to its detail page", async ({ page }) => {
    // The poster image is wrapped in an <a> link to the detail page.
    const firstPosterLink = page.locator("article a:has(img, [class*='aspect-'])").first();
    await firstPosterLink.click();
    await expect(page).toHaveURL(/\/events\/\d+\/.+/);
  });

  // -------------------------------------------------------------------------
  // Specific mock data
  // -------------------------------------------------------------------------

  test("displays a known mock event", async ({ page }) => {
    await expect(
      page.getByText("Spring Record Fair & Swap Meet")
    ).toBeVisible();
  });
});

// ---------------------------------------------------------------------------
// Event detail page
// ---------------------------------------------------------------------------

test.describe("Event detail", () => {
  test.beforeEach(async ({ page }) => {
    // Navigate from the list to the "Spring Record Fair & Swap Meet" detail.
    await page.goto("/events");
    await page.getByText("Spring Record Fair & Swap Meet").click();
    await page.waitForURL(/\/events\/\d+\/spring-record-fair/);
  });

  test("displays the event title as h1", async ({ page }) => {
    await expect(
      page.locator("h1", { hasText: "Spring Record Fair & Swap Meet" })
    ).toBeVisible();
  });

  test("displays the poster image", async ({ page }) => {
    await expect(
      page.locator('img[alt="Spring Record Fair & Swap Meet poster"]')
    ).toBeVisible();
  });

  test("displays DATE & TIME label", async ({ page }) => {
    await expect(page.getByText("DATE & TIME")).toBeVisible();
  });

  test("displays the event date", async ({ page }) => {
    // March 14, 2026 is a Saturday.
    await expect(page.getByText("Saturday, March 14, 2026")).toBeVisible();
  });

  test("displays the time range", async ({ page }) => {
    // 10:00 AM - 4:00 PM Pacific.
    await expect(page.getByText(/10:00 AM\s*-\s*4:00 PM/)).toBeVisible();
  });

  test("displays LOCATION label", async ({ page }) => {
    await expect(page.getByText("LOCATION")).toBeVisible();
  });

  test("displays the venue name", async ({ page }) => {
    await expect(page.getByText("KPBJ Studio Parking Lot")).toBeVisible();
  });

  test("displays the venue address", async ({ page }) => {
    await expect(
      page.getByText("123 Radio Street, Sun Valley, CA 91352")
    ).toBeVisible();
  });

  test("displays the event description", async ({ page }) => {
    await expect(page.getByText(/Dig through crates of vinyl/)).toBeVisible();
  });
});
