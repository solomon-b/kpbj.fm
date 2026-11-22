import { test, expect, type Page } from "@playwright/test";
import { CROSS_BROWSER } from "../playwright.config";

// ---------------------------------------------------------------------------
// Schedule page (/schedule)
// ---------------------------------------------------------------------------
// The schedule uses Alpine.js to toggle between days client-side. All seven
// days are in the DOM but only one is visible at a time via x-show. The
// page opens on today's day index (Mon=0 ... Sun=6).
//
// Mock data has 24/7 coverage. Every day has 12-14 shows filling all hours.

function visibleCards(page: Page) {
  return page.locator("a[href*='/shows/']").locator("visible=true");
}

// The hrefs of the show cards of the day that is visible.
async function visibleHrefs(page: Page): Promise<(string | null)[]> {
  return await visibleCards(page).evaluateAll((els) =>
    els.map((el) => el.getAttribute("href"))
  );
}

// Parse "H:MM AM/PM" into minutes since midnight.
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

test.describe("Schedule page", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/schedule");
  });

  test("displays today's shows in start time order", async ({ page }) => {
    await expect(page.getByRole("heading", { name: "Schedule" })).toBeVisible();
    await expect(page.getByText(/curated mix of ephemeral content/)).toBeVisible();

    // The day label is a <span> with x-show, e.g. "Today, Feb 20". Target
    // <span> to avoid matching the day content div, which also has x-show.
    await expect(page.getByText("Today")).toBeVisible();
    await expect(
      page.locator("span[x-show*='currentDay']").locator("visible=true")
    ).toHaveText(/\w+,\s+\w+ \d+/);

    expect((await visibleHrefs(page)).length).toBeGreaterThanOrEqual(12);
    await expect(visibleCards(page).first().locator("[class*='aspect-']")).toBeVisible();

    // The first show on any day starts at midnight.
    await expect(
      page.getByText("12:00 AM", { exact: true }).locator("visible=true")
    ).toBeVisible();

    const times = await page.locator(".tabular-nums").locator("visible=true").allTextContents();
    const minutes = times.map((t) => toMinutes(t.trim()));
    expect(minutes.length).toBeGreaterThan(1);
    expect(minutes).toEqual([...minutes].sort((a, b) => a - b));
  });

  test("day navigation moves between days and wraps around", { tag: CROSS_BROWSER }, async ({ page }) => {
    const next = page.getByRole("button", { name: "→" });
    const today = page.getByText("Today");
    const todayHrefs = await visibleHrefs(page);

    // Next shows another day with another lineup.
    await next.click();
    await expect(today).not.toBeVisible();
    expect(await visibleHrefs(page)).not.toEqual(todayHrefs);

    // Previous goes back to today.
    await page.getByRole("button", { name: "←" }).click();
    await expect(today).toBeVisible();

    // Seven steps forward come back to today.
    for (let i = 0; i < 7; i++) {
      await next.click();
    }
    await expect(today).toBeVisible();
  });

  test("clicking a show card navigates to the show page", async ({ page }) => {
    await visibleCards(page).first().click();
    await expect(page).toHaveURL(/\/shows\/.+/);
  });
});
