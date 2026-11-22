import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Mobile-specific tests
// ---------------------------------------------------------------------------
// Only the mobile project runs this file (see playwright.config.ts).
// It covers UI patterns that differ from desktop: hamburger menu,
// collapsible filter panel, and simplified player controls.

// ---------------------------------------------------------------------------
// Hamburger menu navigation
// ---------------------------------------------------------------------------
// On mobile the desktop nav is hidden. A hamburger button (☰) opens a
// full-screen overlay with nav links. Each link closes the menu on click.

test.describe("Mobile navigation", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/");
  });

  test("hamburger menu opens and closes", async ({ page }) => {
    await page.locator('button:has-text("☰")').click();

    // The overlay uses x-show="menuOpen" and is a fixed z-50 panel.
    for (const linkText of ["Shows", "Schedule", "Events", "About"]) {
      await expect(page.getByRole("link", { name: linkText }).first()).toBeVisible();
    }

    // Close button renders "×".
    await page.locator('button:has-text("×")').click();
    await expect(page.getByRole("link", { name: "Shows" }).first()).toBeHidden();
  });

  test("hamburger menu link navigates", async ({ page }) => {
    await page.locator('button:has-text("☰")').click();
    await page.getByRole("link", { name: "Shows" }).first().click();
    await page.waitForURL(/\/shows/);
    await expect(page.locator("#shows-list")).toBeVisible();
  });
});

// ---------------------------------------------------------------------------
// Shows filter panel (mobile)
// ---------------------------------------------------------------------------
// On mobile the filter form is hidden behind a "≡ Filter" toggle button.
// Opening it reveals the same form fields as desktop in a stacked layout.

test.describe("Mobile shows filter", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/shows");
  });

  test("search filter works on mobile", async ({ page }) => {
    await page.getByRole("button", { name: /Filter/ }).click();

    // The mobile panel is inside [x-show="filterOpen"]. Scope to it
    // so we don't accidentally match the hidden desktop form.
    const panel = page.locator('[x-show="filterOpen"]');
    await panel.locator("#search").fill("Graveyard Shift");
    await panel.getByRole("button", { name: "Apply" }).click();
    await page.waitForURL(/search=Graveyard/);

    await expect(
      page.locator("#shows-list > a h3", { hasText: "Graveyard Shift" })
    ).toBeVisible();
    await expect(page.locator("#shows-list > a")).toHaveCount(1);
  });

  test("clear button resets filters on mobile", async ({ page }) => {
    // Apply a filter first.
    await page.getByRole("button", { name: /Filter/ }).click();
    const panel = page.locator('[x-show="filterOpen"]');
    await panel.locator("#status").selectOption("Active");
    await panel.getByRole("button", { name: "Apply" }).click();
    await page.waitForURL(/status=active/);

    // Open filter panel again and clear.
    await page.getByRole("button", { name: /Filter/ }).click();
    await page.locator('[x-show="filterOpen"]').getByRole("button", { name: "Clear" }).click();

    await expect(page.locator("#shows-list")).toBeVisible();
    await expect(page).toHaveURL(/\/shows$/);
  });
});

// ---------------------------------------------------------------------------
// Mobile player
// ---------------------------------------------------------------------------
// The mobile player is a fixed bar at the bottom with play/pause and a
// scrolling show name. It does NOT have a volume slider or VOL label.

test.describe("Mobile player", () => {
  test("mobile player has a play button and no volume slider", async ({ page }) => {
    await page.goto("/");

    // Scope to the fixed bottom container to avoid matching the hidden
    // desktop player.
    const mobilePlayer = page.locator(".fixed.bottom-0");
    await expect(mobilePlayer.getByText("[ PLAY ]")).toBeVisible();
    await expect(mobilePlayer.getByRole("slider")).toHaveCount(0);
  });
});
