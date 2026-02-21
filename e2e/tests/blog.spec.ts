import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Blog pages (/blog, /blog/:id/:slug)
// ---------------------------------------------------------------------------
// Mock data has 8 published blog posts with tags. Page size is 10, so all
// posts fit on one page (no infinite scroll needed). Posts are ordered by
// published_at DESC.

test.describe("Blog list", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/blog");
  });

  // -------------------------------------------------------------------------
  // Page structure
  // -------------------------------------------------------------------------

  test("displays the BLOG heading", async ({ page }) => {
    await expect(page.locator("h1", { hasText: "BLOG" })).toBeAttached();
  });

  test("displays blog post cards", async ({ page }) => {
    const cards = page.locator("article");
    const count = await cards.count();
    // Mock data has 8 published blog posts.
    expect(count).toBe(8);
  });

  // -------------------------------------------------------------------------
  // Blog post cards
  // -------------------------------------------------------------------------

  test("post cards have titles", async ({ page }) => {
    const titles = page.locator("article h2");
    const count = await titles.count();
    expect(count).toBe(8);
  });

  test("post cards have hero images", async ({ page }) => {
    // All mock posts have hero images.
    const images = page.locator("article img");
    const count = await images.count();
    expect(count).toBe(8);
  });

  test("post cards have excerpts", async ({ page }) => {
    // Each card has excerpt text below the title/image.
    const firstCard = page.locator("article").first();
    const excerpt = firstCard.locator("p");
    await expect(excerpt).toBeVisible();
    const text = await excerpt.textContent();
    expect(text!.trim().length).toBeGreaterThan(0);
  });

  test("post cards show author name", async ({ page }) => {
    const firstCard = page.locator("article").first();
    // Author name is in a font-bold span.
    const author = firstCard.locator(".font-bold");
    await expect(author.first()).toBeVisible();
  });

  test("post cards show relative time", async ({ page }) => {
    // Published time shows as relative (e.g., "5 months ago").
    const firstCard = page.locator("article").first();
    await expect(firstCard.getByText(/ago/)).toBeVisible();
  });

  test("post cards are clickable links", async ({ page }) => {
    // Each card is wrapped in an <a> tag.
    const firstLink = page.locator("a:has(article)").first();
    const href = await firstLink.getAttribute("href");
    expect(href).toMatch(/\/blog\/\d+\/.+/);
  });

  test("clicking a post card navigates to the detail page", async ({ page }) => {
    const firstLink = page.locator("a:has(article)").first();
    await firstLink.click();
    await expect(page).toHaveURL(/\/blog\/\d+\/.+/);
  });

  // -------------------------------------------------------------------------
  // Specific mock data
  // -------------------------------------------------------------------------

  test("displays a known mock post", async ({ page }) => {
    await expect(
      page.getByText("KPBJ Wins 2025 Community Radio Excellence Award")
    ).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // End of content (all 8 posts fit on one page)
  // -------------------------------------------------------------------------

  test("shows end-of-content indicator", async ({ page }) => {
    await expect(page.locator("#end-of-content")).toBeAttached();
  });
});

// ---------------------------------------------------------------------------
// Blog post detail page
// ---------------------------------------------------------------------------

test.describe("Blog post detail", () => {
  test.beforeEach(async ({ page }) => {
    // Navigate from the list to the award post.
    await page.goto("/blog");
    await page
      .getByText("KPBJ Wins 2025 Community Radio Excellence Award")
      .click();
    await page.waitForURL(/\/blog\/\d+\/kpbj-wins/);
  });

  test("displays the post title as h1", async ({ page }) => {
    await expect(
      page.locator("h1", {
        hasText: "KPBJ Wins 2025 Community Radio Excellence Award",
      })
    ).toBeVisible();
  });

  test("displays the hero image", async ({ page }) => {
    const heroImage = page.locator("article header img");
    await expect(heroImage).toBeVisible();
  });

  test("displays the author name", async ({ page }) => {
    // Author is displayed as "By <name>".
    await expect(page.getByText(/By /)).toBeVisible();
  });

  test("displays the author avatar initials", async ({ page }) => {
    // Avatar shows first 2 characters of author display name in a circle.
    const avatar = page.locator(".rounded-full").first();
    await expect(avatar).toBeVisible();
  });

  test("displays the publication date", async ({ page }) => {
    // Published August 4, 2025.
    await expect(page.getByText("August 04, 2025")).toBeVisible();
  });

  test("displays tags", async ({ page }) => {
    // This post has "award" and "community" tags.
    await expect(page.getByText("#award")).toBeVisible();
    await expect(page.getByText("#community")).toBeVisible();
  });

  test("clicking a tag navigates to filtered blog list", async ({ page }) => {
    await page.getByText("#award").click();
    await expect(page).toHaveURL(/\/blog\?tag=award/);
  });

  test("tag filter shows only matching posts", async ({ page }) => {
    // Click the #award tag — only the KPBJ Wins award post has this tag.
    await page.getByText("#award").click();
    await page.waitForURL(/\/blog\?tag=award/);

    const cards = page.locator("article");
    await expect(cards).toHaveCount(1);
    await expect(
      page.getByText("KPBJ Wins 2025 Community Radio Excellence Award")
    ).toBeVisible();
  });

  test("displays the article content", async ({ page }) => {
    // The markdown content includes this text.
    await expect(
      page.getByText(/thrilled to announce/)
    ).toBeVisible();
  });

  test("displays the author bio section", async ({ page }) => {
    // Footer has a bio card with the author's role info.
    const footer = page.locator("article footer");
    await expect(footer).toBeVisible();
  });

  test("has a back to blog button", async ({ page }) => {
    await expect(page.getByText("← BACK TO BLOG")).toBeVisible();
  });

  test("back to blog button navigates to the list", async ({ page }) => {
    await page.getByText("← BACK TO BLOG").click();
    await expect(page).toHaveURL(/\/blog$/);
  });
});
