import { test, expect, type Page } from "@playwright/test";

// ---------------------------------------------------------------------------
// Blog pages (/blog, /blog/:id/:slug)
// ---------------------------------------------------------------------------
// Mock data has 8 published blog posts with tags. Page size is 10, so all
// posts fit on one page (no infinite scroll needed). Posts are ordered by
// published_at DESC.

const AWARD_POST = "KPBJ Wins 2025 Community Radio Excellence Award";

test.describe("Blog list", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/blog");
  });

  test("displays the post cards", async ({ page }) => {
    await expect(page.locator("h1", { hasText: "BLOG" })).toBeAttached();

    // All 8 mock posts have a title and a hero image.
    await expect(page.locator("article")).toHaveCount(8);
    await expect(page.locator("article h2")).toHaveCount(8);
    await expect(page.locator("article img")).toHaveCount(8);
    await expect(page.getByText(AWARD_POST)).toBeVisible();

    // Each card has an excerpt, the author in a font-bold span, and a
    // relative time such as "5 months ago".
    const firstCard = page.locator("article").first();
    await expect(firstCard.locator("p")).toHaveText(/\S/);
    await expect(firstCard.locator(".font-bold").first()).toBeVisible();
    await expect(firstCard.getByText(/ago/)).toBeVisible();

    // All 8 posts fit on one page.
    await expect(page.locator("#end-of-content")).toBeAttached();
  });

  test("clicking a post card navigates to the detail page", async ({ page }) => {
    await page.locator("a:has(article)").first().click();
    await expect(page).toHaveURL(/\/blog\/\d+\/.+/);
  });
});

// ---------------------------------------------------------------------------
// Blog post detail page
// ---------------------------------------------------------------------------

// The post id comes from the database, so the test opens the post from the list.
async function openAwardPost(page: Page) {
  await page.goto("/blog");
  await page.getByText(AWARD_POST).click();
  await page.waitForURL(/\/blog\/\d+\/kpbj-wins/);
}

test.describe("Blog post detail", () => {
  test.beforeEach(async ({ page }) => {
    await openAwardPost(page);
  });

  test("displays the post", async ({ page }) => {
    await expect(page.locator("h1", { hasText: AWARD_POST })).toBeVisible();
    await expect(page.locator("article header img")).toBeVisible();

    // The author is "By <name>", with an avatar of the first 2 characters.
    await expect(page.getByText(/By /)).toBeVisible();
    await expect(page.locator(".rounded-full").first()).toBeVisible();

    await expect(page.getByText("August 04, 2025")).toBeVisible();
    await expect(page.getByText("#award")).toBeVisible();
    await expect(page.getByText("#community")).toBeVisible();
    await expect(page.getByText(/thrilled to announce/)).toBeVisible();

    // The footer has a bio card with the author's role info.
    await expect(page.locator("article footer")).toBeVisible();
  });

  test("clicking a tag shows only the matching posts", async ({ page }) => {
    // Only the award post has the #award tag.
    await page.getByText("#award").click();
    await page.waitForURL(/\/blog\?tag=award/);
    await expect(page.locator("article")).toHaveCount(1);
    await expect(page.getByText(AWARD_POST)).toBeVisible();
  });

  test("back to blog button navigates to the list", async ({ page }) => {
    await page.getByText("← BACK TO BLOG").click();
    await expect(page).toHaveURL(/\/blog$/);
  });
});
