import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Shows index page (/shows)
// ---------------------------------------------------------------------------
// Listing page with filter panel, show cards in a grid, and infinite scroll.
// Tests assume mock data is loaded (just dev-mock-data).

test.describe("Shows index", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/shows");
  });

  // -------------------------------------------------------------------------
  // Page structure
  // -------------------------------------------------------------------------

  test("displays the SHOWS heading", async ({ page }) => {
    // pageHeader renders an h1 with "SHOWS". It has desktop:hidden so it's
    // not visible at the 1280px default viewport. getByRole skips hidden
    // elements, so we use a CSS locator and toBeAttached() which checks
    // DOM presence regardless of visibility.
    await expect(page.locator("h1", { hasText: "SHOWS" })).toBeAttached();
  });

  // -------------------------------------------------------------------------
  // Filter panel (desktop only — hidden behind toggle on mobile)
  // -------------------------------------------------------------------------
  // The filter form is rendered twice (mobile + desktop) with the same IDs.
  // On mobile the form is behind a "≡ Filter" toggle button.
  // See mobile.spec.ts for mobile filter tests.
  test.describe("Filter panel", () => {
  test.beforeEach(async ({}, testInfo) => {
    test.skip(testInfo.project.name.startsWith("mobile"), "Desktop filter — see mobile.spec.ts");
  });

  test("has a search input", async ({ page }) => {
    // Two copies of the filter form exist (mobile + desktop). We target
    // the visible one by ID + visibility filter.
    const search = page.locator("#search").locator("visible=true");
    await expect(search).toBeVisible();
    await expect(search).toHaveAttribute("placeholder", "Search shows...");
  });

  test("has a status filter with correct options", async ({ page }) => {
    const status = page.locator("#status").locator("visible=true");
    await expect(status).toBeVisible();

    // toHaveValues would check selected value. Instead we verify all
    // <option> elements exist inside the select.
    // locator("option") finds child elements matching a CSS selector.
    const options = status.locator("option");
    await expect(options).toHaveText(["All Shows", "Active", "Inactive"]);
  });

  test("has a sort dropdown with correct options", async ({ page }) => {
    const sortBy = page.locator("#sortBy").locator("visible=true");
    await expect(sortBy).toBeVisible();

    const options = sortBy.locator("option");
    await expect(options).toHaveText([
      "Name (A-Z)",
      "Name (Z-A)",
      "Newest First",
      "Oldest First",
    ]);
  });

  test("has a tag filter with populated options", async ({ page }) => {
    const tag = page.locator("#tag").locator("visible=true");
    await expect(tag).toBeVisible();

    // First option is always "All Tags".
    const firstOption = tag.locator("option").first();
    await expect(firstOption).toHaveText("All Tags");

    // Should have more options populated from the database.
    // Each tag option shows "Tag Name (count)" format.
    const optionCount = await tag.locator("option").count();
    expect(optionCount).toBeGreaterThan(1);

    // Verify at least one tag option matches the "Name (N)" pattern.
    const secondOption = await tag.locator("option").nth(1).textContent();
    expect(secondOption).toMatch(/.+ \(\d+\)/);
  });

  test("has Apply and Clear buttons", async ({ page }) => {
    await expect(
      page.getByRole("button", { name: "Apply" }).locator("visible=true")
    ).toBeVisible();
    await expect(
      page.getByRole("button", { name: "Clear" }).locator("visible=true")
    ).toBeVisible();
  });
  }); // end Filter panel

  // -------------------------------------------------------------------------
  // Show cards
  // -------------------------------------------------------------------------

  test("displays show cards in the grid", async ({ page }) => {
    const cards = page.locator("#shows-list > a");
    const count = await cards.count();
    expect(count).toBeGreaterThan(0);
  });

  test("each show card has a title", async ({ page }) => {
    // Every card should contain an <h3> with the show name.
    const titles = page.locator("#shows-list > a h3");
    const count = await titles.count();
    expect(count).toBeGreaterThan(0);

    // Verify the first title has non-empty text.
    const firstTitle = await titles.first().textContent();
    expect(firstTitle?.trim().length).toBeGreaterThan(0);
  });

  test("each show card has an image or placeholder", async ({ page }) => {
    // Cards have either an <img> (if logo exists) or a placeholder <div>.
    // The image container always exists with aspect-[4/3].
    const firstCard = page.locator("#shows-list > a").first();
    const imageContainer = firstCard.locator("div").first();
    await expect(imageContainer).toBeVisible();
  });

  test("show card images have descriptive alt text", async ({ page }) => {
    // Cards with images render alt text in "Show Title logo" format.
    // Not all cards may have images (some have placeholders), so find one
    // that does and verify its alt text.
    const images = page.locator("#shows-list > a img");
    const imgCount = await images.count();

    if (imgCount > 0) {
      const alt = await images.first().getAttribute("alt");
      expect(alt).toMatch(/.+ logo$/);
    }
  });

  test("show cards link to detail pages", async ({ page }) => {
    const firstCard = page.locator("#shows-list > a").first();
    const href = await firstCard.getAttribute("href");
    expect(href).toMatch(/\/shows\/.+/);
  });

  test("clicking a show card navigates to the show page", async ({ page }) => {
    const firstCard = page.locator("#shows-list > a").first();
    await firstCard.click();
    await page.waitForURL(/\/shows\/.+/);
  });

  // -------------------------------------------------------------------------
  // Filter behavior (desktop only)
  // -------------------------------------------------------------------------
  test.describe("Filter behavior", () => {
  test.beforeEach(async ({}, testInfo) => {
    test.skip(testInfo.project.name.startsWith("mobile"), "Desktop filter — see mobile.spec.ts");
  });

  test("applying status filter updates URL", async ({ page }) => {
    await page.locator("#status").locator("visible=true").selectOption("Active");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();

    // The JS builds a URL like /shows?status=active and pushes it.
    await page.waitForURL(/status=active/);
  });

  test("applying sort filter updates URL", async ({ page }) => {
    await page
      .locator("#sortBy")
      .locator("visible=true")
      .selectOption("Newest First");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();

    await page.waitForURL(/sortBy=created_newest/);
  });

  test("search filter updates URL", async ({ page }) => {
    await page.locator("#search").locator("visible=true").fill("jazz");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();

    await page.waitForURL(/search=jazz/);
  });

  test("search returns matching shows in results", async ({ page }) => {
    // Search for a specific mock data show with a unique name.
    await page
      .locator("#search")
      .locator("visible=true")
      .fill("Graveyard Shift");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();
    await page.waitForURL(/search=Graveyard/);

    // The matching show card should appear in the results.
    await expect(page.locator("#shows-list > a h3", { hasText: "Graveyard Shift" })).toBeVisible();

    // Only one show should match this specific name.
    const cards = page.locator("#shows-list > a");
    await expect(cards).toHaveCount(1);
  });

  test("search returns multiple matching shows", async ({ page }) => {
    // "Folk" matches two mock data shows: "Folk Tales" and "Folk Underground".
    // Search is ILIKE against title and description.
    await page.locator("#search").locator("visible=true").fill("Folk");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();
    await page.waitForURL(/search=Folk/);

    const cards = page.locator("#shows-list > a");
    await expect(cards).toHaveCount(2);

    // Both show titles should be present.
    await expect(page.locator("#shows-list > a h3", { hasText: "Folk Tales" })).toBeVisible();
    await expect(page.locator("#shows-list > a h3", { hasText: "Folk Underground" })).toBeVisible();
  });

  test("tag filter updates URL", async ({ page }) => {
    const tagSelect = page.locator("#tag").locator("visible=true");

    // Pick the second option (first real tag after "All Tags").
    const secondOption = tagSelect.locator("option").nth(1);
    const tagValue = await secondOption.getAttribute("value");

    await tagSelect.selectOption(tagValue!);
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();

    await page.waitForURL(new RegExp(`tag=${tagValue}`));
  });

  test("Clear button resets filters and URL", async ({ page }) => {
    // Apply a filter first.
    await page.locator("#status").locator("visible=true").selectOption("Active");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();
    await page.waitForURL(/status=active/);

    // Click Clear — resets the form, pushes clean URL, fetches unfiltered shows.
    await page.getByRole("button", { name: "Clear" }).locator("visible=true").click();

    // Wait for the HTMX swap to complete.
    await expect(page.locator("#shows-list")).toBeVisible();

    // Verify URL is clean (no query params).
    // toHaveURL accepts a string or regex and auto-waits.
    await expect(page).toHaveURL(/\/shows$/);

    // Verify the form was reset — status dropdown back to default.
    await expect(
      page.locator("#status").locator("visible=true")
    ).toHaveValue("");
  });

  // -------------------------------------------------------------------------
  // Filter persistence — inputs retain values after applying
  // -------------------------------------------------------------------------

  test("filter inputs retain selected values after applying", async ({ page }) => {
    // Set all three simple filters.
    await page.locator("#search").locator("visible=true").fill("morning");
    await page.locator("#status").locator("visible=true").selectOption("Active");
    await page.locator("#sortBy").locator("visible=true").selectOption("Newest First");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();

    // Wait for the HTMX swap to complete.
    await page.waitForURL(/search=morning/);

    // After the swap, the server re-renders the form with persisted values.
    await expect(
      page.locator("#search").locator("visible=true")
    ).toHaveValue("morning");
    await expect(
      page.locator("#status").locator("visible=true")
    ).toHaveValue("active");
    await expect(
      page.locator("#sortBy").locator("visible=true")
    ).toHaveValue("created_newest");
  });

  // -------------------------------------------------------------------------
  // URL-driven filter state — navigating directly pre-populates form
  // -------------------------------------------------------------------------

  test("navigating to URL with query params pre-populates filters", async ({ page }) => {
    // Go directly to a filtered URL.
    await page.goto("/shows?status=active&sortBy=name_za");

    // The server renders the form with these values pre-selected.
    await expect(
      page.locator("#status").locator("visible=true")
    ).toHaveValue("active");
    await expect(
      page.locator("#sortBy").locator("visible=true")
    ).toHaveValue("name_za");
  });

  test("navigating to URL with search param pre-fills search input", async ({ page }) => {
    await page.goto("/shows?search=jazz");

    await expect(
      page.locator("#search").locator("visible=true")
    ).toHaveValue("jazz");
  });

  // -------------------------------------------------------------------------
  // Browser back/forward
  // -------------------------------------------------------------------------

  test("browser back button restores previous filter state", async ({ page }) => {
    // Apply a filter.
    await page.locator("#status").locator("visible=true").selectOption("Active");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();
    await page.waitForURL(/status=active/);

    // Apply a different filter.
    await page.locator("#status").locator("visible=true").selectOption("Inactive");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();
    await page.waitForURL(/status=inactive/);

    // Go back — should return to the "active" URL.
    await page.goBack();
    await expect(page).toHaveURL(/status=active/);
  });

  // -------------------------------------------------------------------------
  // Combined filters
  // -------------------------------------------------------------------------

  test("combining status and sort filters", async ({ page }) => {
    // Apply two filters at once — status + sort.
    await page.locator("#status").locator("visible=true").selectOption("Active");
    await page
      .locator("#sortBy")
      .locator("visible=true")
      .selectOption("Newest First");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();

    // Both params should be in the URL.
    await page.waitForURL(/status=active/);
    expect(page.url()).toContain("sortBy=created_newest");
  });

  test("combining search with status filter", async ({ page }) => {
    await page.locator("#search").locator("visible=true").fill("morning");
    await page.locator("#status").locator("visible=true").selectOption("Active");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();

    await page.waitForURL(/search=morning/);
    expect(page.url()).toContain("status=active");
  });

  test("combining tag with status filter", async ({ page }) => {
    // Pick the first real tag.
    const tagSelect = page.locator("#tag").locator("visible=true");
    const tagValue = await tagSelect.locator("option").nth(1).getAttribute("value");
    await tagSelect.selectOption(tagValue!);

    await page.locator("#status").locator("visible=true").selectOption("Active");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();

    await page.waitForURL(new RegExp(`tag=${tagValue}`));
    expect(page.url()).toContain("status=active");
  });

  test("combining all filters: search + tag + status + sort", async ({ page }) => {
    const tagSelect = page.locator("#tag").locator("visible=true");
    const tagValue = await tagSelect.locator("option").nth(1).getAttribute("value");

    await page.locator("#search").locator("visible=true").fill("the");
    await tagSelect.selectOption(tagValue!);
    await page.locator("#status").locator("visible=true").selectOption("Active");
    await page.locator("#sortBy").locator("visible=true").selectOption("Name (Z-A)");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();

    await page.waitForURL(/search=the/);
    expect(page.url()).toContain(`tag=${tagValue}`);
    expect(page.url()).toContain("status=active");
    expect(page.url()).toContain("sortBy=name_za");
  });

  // -------------------------------------------------------------------------
  // Empty state
  // -------------------------------------------------------------------------

  test("shows empty state when no results match", async ({ page }) => {
    // Search for a string that won't match any show in the mock data.
    await page.locator("#search").locator("visible=true").fill("zzznonexistent999");
    await page.getByRole("button", { name: "Apply" }).locator("visible=true").click();

    // Wait for the HTMX swap to complete.
    await page.waitForURL(/search=zzznonexistent999/);

    // The template renders "No Shows Found" when the result list is empty.
    await expect(page.getByRole("heading", { name: "No Shows Found" })).toBeVisible();
    await expect(page.getByText("Check back soon for new shows!")).toBeVisible();

    // The shows grid should not be present.
    await expect(page.locator("#shows-list")).not.toBeAttached();
  });
  }); // end Filter behavior

  // -------------------------------------------------------------------------
  // Infinite scroll
  // -------------------------------------------------------------------------
  // With 104 shows and a page size of 12, the first page always has a
  // #load-more-sentinel that triggers loading the next page when scrolled
  // into view.

  test("first page shows the load-more sentinel", async ({ page }) => {
    // 104 shows / 12 per page = more pages available, so sentinel exists.
    await expect(page.locator("#load-more-sentinel")).toBeAttached();
  });

  test("scrolling to bottom loads more show cards", async ({ page }) => {
    // Count initial cards (should be 12).
    const initialCount = await page.locator("#shows-list > a").count();
    expect(initialCount).toBe(12);

    // Scroll the sentinel into view to trigger the HTMX infinite scroll.
    await page.locator("#load-more-sentinel").scrollIntoViewIfNeeded();

    // Wait for new cards to appear. After loading, we should have 24.
    await expect(page.locator("#shows-list > a")).toHaveCount(24, {
      timeout: 10_000,
    });
  });
});
