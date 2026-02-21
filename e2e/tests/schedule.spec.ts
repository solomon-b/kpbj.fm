import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Schedule page (/schedule)
// ---------------------------------------------------------------------------
// The schedule uses Alpine.js to toggle between days client-side. All seven
// days are in the DOM but only one is visible at a time via x-show. The
// page opens on today's day index (Mon=0 ... Sun=6).
//
// Mock data has 24/7 coverage — every day has 12-14 shows filling all hours.

test.describe("Schedule page", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/schedule");
  });

  // -------------------------------------------------------------------------
  // Page structure
  // -------------------------------------------------------------------------

  test("displays the Schedule heading", async ({ page }) => {
    await expect(
      page.getByRole("heading", { name: "Schedule" })
    ).toBeVisible();
  });

  test("displays the ephemeral content note", async ({ page }) => {
    await expect(
      page.getByText(/curated mix of ephemeral content/)
    ).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Day navigation
  // -------------------------------------------------------------------------

  test("shows today label on initial load", async ({ page }) => {
    // The current day label includes "Today" text.
    await expect(page.getByText("Today")).toBeVisible();
  });

  test("next button advances to a different day", async ({ page }) => {
    // "Today" should be visible initially.
    await expect(page.getByText("Today")).toBeVisible();

    // Click next — should show a day name instead of "Today".
    await page.getByRole("button", { name: "→" }).click();
    await expect(page.getByText("Today")).not.toBeVisible();
  });

  test("prev button goes to the previous day", async ({ page }) => {
    await page.getByRole("button", { name: "→" }).click();
    await expect(page.getByText("Today")).not.toBeVisible();

    // Click prev — should return to "Today".
    await page.getByRole("button", { name: "←" }).click();
    await expect(page.getByText("Today")).toBeVisible();
  });

  test("day navigation wraps around", async ({ page }) => {
    // Click next 7 times — should cycle back to "Today".
    const next = page.getByRole("button", { name: "→" });
    for (let i = 0; i < 7; i++) {
      await next.click();
    }
    await expect(page.getByText("Today")).toBeVisible();
  });

  test("day label includes a date", async ({ page }) => {
    // The day label is a <span> with x-show. It shows "Today, Feb 20" or
    // "Mon, Feb 23" etc. Target <span> specifically to avoid matching the
    // day content div which also has x-show.
    const dayLabel = page.locator("span[x-show*='currentDay']").locator("visible=true");
    const text = await dayLabel.textContent();
    expect(text).toMatch(/\w+,\s+\w+ \d+/);
  });

  // -------------------------------------------------------------------------
  // Show cards
  // -------------------------------------------------------------------------

  test("displays show cards for the current day", async ({ page }) => {
    // Every day in mock data has 12-14 shows. Show cards are <a> links
    // inside the visible day panel.
    const visibleCards = page.locator("a[href*='/shows/']").locator("visible=true");
    const count = await visibleCards.count();
    expect(count).toBeGreaterThanOrEqual(12);
  });

  test("show cards display start times", async ({ page }) => {
    // Times are rendered in 12-hour format (e.g., "12:00 AM", "2:00 AM").
    // The first show on any day starts at 12:00 AM (midnight).
    await expect(
      page.getByText("12:00 AM", { exact: true }).locator("visible=true")
    ).toBeVisible();
  });

  test("show cards display show titles", async ({ page }) => {
    // Cards have a title div with font-bold styling.
    const visibleTitles = page.locator(".font-bold").locator("visible=true");
    const count = await visibleTitles.count();
    // At least the show titles + heading (lots of bold elements).
    expect(count).toBeGreaterThan(1);
  });

  test("show cards have images or placeholders", async ({ page }) => {
    // Each card has an aspect-[4/3] image container.
    const firstCard = page.locator("a[href*='/shows/']").locator("visible=true").first();
    const imageContainer = firstCard.locator("[class*='aspect-']");
    await expect(imageContainer).toBeVisible();
  });

  test("clicking a show card navigates to the show page", async ({ page }) => {
    const firstCard = page.locator("a[href*='/shows/']").locator("visible=true").first();
    await firstCard.click();
    await expect(page).toHaveURL(/\/shows\/.+/);
  });

  // -------------------------------------------------------------------------
  // Day switching shows different content
  // -------------------------------------------------------------------------

  test("different days show different shows", async ({ page }) => {
    // Collect all visible show hrefs on today's view.
    const todayCards = page.locator("a[href*='/shows/']").locator("visible=true");
    const todayHrefs: string[] = [];
    for (let i = 0; i < await todayCards.count(); i++) {
      todayHrefs.push((await todayCards.nth(i).getAttribute("href"))!);
    }

    // Navigate to the next day.
    await page.getByRole("button", { name: "→" }).click();

    // Collect the next day's show hrefs.
    const nextCards = page.locator("a[href*='/shows/']").locator("visible=true");
    const nextHrefs: string[] = [];
    for (let i = 0; i < await nextCards.count(); i++) {
      nextHrefs.push((await nextCards.nth(i).getAttribute("href"))!);
    }

    // The two days should have different show lineups.
    expect(nextHrefs).not.toEqual(todayHrefs);
  });

  // -------------------------------------------------------------------------
  // Show ordering
  // -------------------------------------------------------------------------

  test("shows are ordered by start time", async ({ page }) => {
    // Grab all visible time elements (12-hour AM/PM format).
    const timeElements = page.locator(".tabular-nums").locator("visible=true");
    const count = await timeElements.count();
    expect(count).toBeGreaterThan(1);

    // Parse "H:MM AM/PM" into minutes since midnight for comparison.
    function toMinutes(timeStr: string): number {
      const match = timeStr.match(/(\d+):(\d+)\s*(AM|PM)/i);
      if (!match) return -1;
      let hours = parseInt(match[1]);
      const mins = parseInt(match[2]);
      const period = match[3].toUpperCase();
      if (period === "AM" && hours === 12) hours = 0;
      if (period === "PM" && hours !== 12) hours += 12;
      return hours * 60 + mins;
    }

    const minutes: number[] = [];
    for (let i = 0; i < count; i++) {
      const text = await timeElements.nth(i).textContent();
      if (text) minutes.push(toMinutes(text.trim()));
    }

    // Verify times are in ascending order.
    for (let i = 1; i < minutes.length; i++) {
      expect(minutes[i]).toBeGreaterThanOrEqual(minutes[i - 1]);
    }
  });
});
