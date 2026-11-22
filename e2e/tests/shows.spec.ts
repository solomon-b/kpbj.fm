import { test, expect, type Page } from "@playwright/test";
import { CROSS_BROWSER } from "../playwright.config";

// ---------------------------------------------------------------------------
// Shows index page (/shows)
// ---------------------------------------------------------------------------
// Listing page with filter panel, show cards in a grid, and infinite scroll.
// Tests assume mock data is loaded (just dev-mock-data).
//
// Desktop layout only. See mobile.spec.ts for the mobile filter panel.
//
// The filter form is rendered twice (mobile + desktop) with the same IDs,
// so each locator picks the visible copy.

function field(page: Page, id: string) {
  return page.locator(`#${id}`).locator("visible=true");
}

async function apply(page: Page) {
  await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();
}

test.describe("Shows index", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/shows");
  });

  // -------------------------------------------------------------------------
  // Page structure
  // -------------------------------------------------------------------------

  test("renders the show cards and the filter form", async ({ page }) => {
    // pageHeader renders an h1 with "SHOWS". It has desktop:hidden, so the
    // test checks DOM presence and not visibility.
    await expect(page.locator("h1", { hasText: "SHOWS" })).toBeAttached();

    // With 104 shows and a page size of 12, the first page has 12 cards
    // and a #load-more-sentinel for infinite scroll.
    const cards = page.locator("#shows-list > a");
    await expect(cards).toHaveCount(12);
    expect((await cards.first().locator("h3").textContent())?.trim().length).toBeGreaterThan(0);
    await expect(cards.first()).toHaveAttribute("href", /\/shows\/.+/);
    await expect(page.locator("#load-more-sentinel")).toBeAttached();

    await expect(field(page, "search")).toHaveAttribute("placeholder", "Search shows...");
    await expect(field(page, "status").locator("option")).toHaveText([
      "All Shows",
      "Active",
      "Inactive",
    ]);
    await expect(field(page, "sortBy").locator("option")).toHaveText([
      "Name (A-Z)",
      "Name (Z-A)",
      "Newest First",
      "Oldest First",
    ]);

    // The first tag option is "All Tags". The others come from the
    // database in "Tag Name (count)" format.
    const tagOptions = field(page, "tag").locator("option");
    await expect(tagOptions.first()).toHaveText("All Tags");
    await expect(tagOptions.nth(1)).toHaveText(/.+ \(\d+\)/);

    await expect(page.getByRole("button", { name: "Clear" }).locator("visible=true")).toBeVisible();
  });

  test("clicking a show card navigates to the show page", async ({ page }) => {
    await page.locator("#shows-list > a").first().click();
    await page.waitForURL(/\/shows\/.+/);
  });

  // -------------------------------------------------------------------------
  // Filter behavior
  // -------------------------------------------------------------------------

  test("search returns the matching shows", async ({ page }) => {
    const cards = page.locator("#shows-list > a");

    // A unique mock show name matches one show.
    await field(page, "search").fill("Graveyard Shift");
    await apply(page);
    await page.waitForURL(/search=Graveyard/);
    await expect(cards).toHaveCount(1);
    await expect(page.locator("#shows-list > a h3", { hasText: "Graveyard Shift" })).toBeVisible();

    // "Folk" matches "Folk Tales" and "Folk Underground". Search is ILIKE
    // against title and description.
    await field(page, "search").fill("Folk");
    await apply(page);
    await page.waitForURL(/search=Folk/);
    await expect(cards).toHaveCount(2);
    await expect(page.locator("#shows-list > a h3", { hasText: "Folk Tales" })).toBeVisible();
    await expect(page.locator("#shows-list > a h3", { hasText: "Folk Underground" })).toBeVisible();
  });

  test("all filters go into the URL and stay in the form", { tag: CROSS_BROWSER }, async ({ page }) => {
    const tagValue = await field(page, "tag").locator("option").nth(1).getAttribute("value");

    await field(page, "search").fill("the");
    await field(page, "tag").selectOption(tagValue!);
    await field(page, "status").selectOption("Active");
    await field(page, "sortBy").selectOption("Name (Z-A)");
    await apply(page);

    // The client code builds the URL and pushes it.
    await page.waitForURL(/search=the/);
    expect(page.url()).toContain(`tag=${tagValue}`);
    expect(page.url()).toContain("status=active");
    expect(page.url()).toContain("sortBy=name_za");

    // After the swap, the server re-renders the form with the values.
    await expect(field(page, "search")).toHaveValue("the");
    await expect(field(page, "tag")).toHaveValue(tagValue!);
    await expect(field(page, "status")).toHaveValue("active");
    await expect(field(page, "sortBy")).toHaveValue("name_za");
  });

  test("URL query params pre-populate the filters", async ({ page }) => {
    await page.goto("/shows?status=active&sortBy=name_za&search=jazz");

    await expect(field(page, "search")).toHaveValue("jazz");
    await expect(field(page, "status")).toHaveValue("active");
    await expect(field(page, "sortBy")).toHaveValue("name_za");
  });

  test("Clear button resets filters and URL", { tag: CROSS_BROWSER }, async ({ page }) => {
    await field(page, "status").selectOption("Active");
    await apply(page);
    await page.waitForURL(/status=active/);

    // Clear resets the form, pushes a clean URL, and fetches unfiltered shows.
    await page.getByRole("button", { name: "Clear" }).locator("visible=true").click();
    await expect(page.locator("#shows-list")).toBeVisible();
    await expect(page).toHaveURL(/\/shows$/);
    await expect(field(page, "status")).toHaveValue("");
  });

  test("browser back button restores previous filter state", { tag: CROSS_BROWSER }, async ({ page }) => {
    await field(page, "status").selectOption("Active");
    await apply(page);
    await page.waitForURL(/status=active/);

    await field(page, "status").selectOption("Inactive");
    await apply(page);
    await page.waitForURL(/status=inactive/);

    await page.goBack();
    await expect(page).toHaveURL(/status=active/);
  });

  test("shows empty state when no results match", async ({ page }) => {
    await field(page, "search").fill("zzznonexistent999");
    await apply(page);
    await page.waitForURL(/search=zzznonexistent999/);

    await expect(page.getByRole("heading", { name: "No Shows Found" })).toBeVisible();
    await expect(page.getByText("Check back soon for new shows!")).toBeVisible();
    await expect(page.locator("#shows-list")).not.toBeAttached();
  });

  // -------------------------------------------------------------------------
  // Infinite scroll
  // -------------------------------------------------------------------------

  test("scrolling to bottom loads more show cards", { tag: CROSS_BROWSER }, async ({ page }) => {
    await expect(page.locator("#shows-list > a")).toHaveCount(12);

    // Scroll the sentinel into view to trigger the HTMX infinite scroll.
    await page.locator("#load-more-sentinel").scrollIntoViewIfNeeded();
    await expect(page.locator("#shows-list > a")).toHaveCount(24, {
      timeout: 10_000,
    });
  });
});
