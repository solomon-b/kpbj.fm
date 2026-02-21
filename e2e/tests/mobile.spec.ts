import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Mobile-specific tests
// ---------------------------------------------------------------------------
// These tests only run on the mobile project (375x667 viewport).
// They cover UI patterns that differ from desktop: hamburger menu,
// collapsible filter panel, and simplified player controls.

test.beforeEach(async ({}, testInfo) => {
  test.skip(!testInfo.project.name.startsWith("mobile"), "Mobile-only tests");
});

// ---------------------------------------------------------------------------
// Hamburger menu navigation
// ---------------------------------------------------------------------------
// On mobile the desktop nav is hidden. A hamburger button (☰) opens a
// full-screen overlay with nav links. Each link closes the menu on click.

test.describe("Mobile navigation", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/");
  });

  test("hamburger menu button is visible", async ({ page }) => {
    await expect(page.locator('button:has-text("☰")')).toBeVisible();
  });

  test("hamburger menu opens and shows nav links", async ({ page }) => {
    await page.locator('button:has-text("☰")').click();

    // The overlay uses x-show="menuOpen" and is a fixed z-50 panel.
    for (const linkText of ["Shows", "Schedule", "Events", "About"]) {
      await expect(
        page.getByRole("link", { name: linkText }).first()
      ).toBeVisible();
    }
  });

  test("hamburger menu close button works", async ({ page }) => {
    await page.locator('button:has-text("☰")').click();

    // Wait for menu to be visible.
    await expect(
      page.getByRole("link", { name: "Shows" }).first()
    ).toBeVisible();

    // Close button renders "×".
    await page.locator('button:has-text("×")').click();

    // Menu should close — nav links no longer visible.
    await expect(
      page.getByRole("link", { name: "Shows" }).first()
    ).toBeHidden();
  });

  test("navigate to Shows via hamburger menu", async ({ page }) => {
    await page.locator('button:has-text("☰")').click();
    await page.getByRole("link", { name: "Shows" }).first().click();
    await page.waitForURL(/\/shows/);
    await expect(page.locator("#shows-list")).toBeVisible();
  });

  test("navigate to Schedule via hamburger menu", async ({ page }) => {
    await page.locator('button:has-text("☰")').click();
    await page.getByRole("link", { name: "Schedule" }).first().click();
    await page.waitForURL(/\/schedule/);
  });

  test("navigate to Events via hamburger menu", async ({ page }) => {
    await page.locator('button:has-text("☰")').click();
    await page.getByRole("link", { name: "Events" }).first().click();
    await page.waitForURL(/\/events/);
  });

  test("navigate to About via hamburger menu", async ({ page }) => {
    await page.locator('button:has-text("☰")').click();
    await page.getByRole("link", { name: "About" }).first().click();
    await page.waitForURL(/\/about/);
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

  test("filter toggle button is visible", async ({ page }) => {
    await expect(page.getByRole("button", { name: /Filter/ })).toBeVisible();
  });

  test("filter toggle opens the filter panel", async ({ page }) => {
    await page.getByRole("button", { name: /Filter/ }).click();

    // The mobile panel is inside [x-show="filterOpen"]. Scope to it
    // so we don't accidentally match the hidden desktop form.
    const panel = page.locator('[x-show="filterOpen"]');
    await expect(panel.locator("#search")).toBeVisible();
  });

  test("search filter works on mobile", async ({ page }) => {
    await page.getByRole("button", { name: /Filter/ }).click();

    const panel = page.locator('[x-show="filterOpen"]');
    await panel.locator("#search").fill("Graveyard Shift");
    await panel.getByRole("button", { name: "Apply" }).click();
    await page.waitForURL(/search=Graveyard/);

    await expect(
      page.locator("#shows-list > a h3", { hasText: "Graveyard Shift" })
    ).toBeVisible();
    const cards = page.locator("#shows-list > a");
    await expect(cards).toHaveCount(1);
  });

  test("status filter works on mobile", async ({ page }) => {
    await page.getByRole("button", { name: /Filter/ }).click();

    const panel = page.locator('[x-show="filterOpen"]');
    await panel.locator("#status").selectOption("Active");
    await panel.getByRole("button", { name: "Apply" }).click();
    await page.waitForURL(/status=active/);
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
    const panel2 = page.locator('[x-show="filterOpen"]');
    await panel2.getByRole("button", { name: "Clear" }).click();

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
  // The mobile player is a fixed bar at the bottom of the viewport.
  // It uses two <span> elements toggled by x-show for play/pause,
  // unlike the desktop player which uses x-text on a single button.
  // We scope to the fixed bottom container to avoid matching the
  // hidden desktop player.

  test.beforeEach(async ({ page }) => {
    await page.goto("/");
  });

  test("mobile player has play button", async ({ page }) => {
    // The mobile player is inside a fixed bottom-0 container.
    const mobilePlayer = page.locator(".fixed.bottom-0");
    await expect(mobilePlayer.getByText("[ PLAY ]")).toBeVisible();
  });

  test("mobile player does not show volume slider", async ({ page }) => {
    // The desktop player has a range slider for volume.
    // On mobile, no slider is rendered — the desktop player is hidden.
    const mobilePlayer = page.locator(".fixed.bottom-0");
    await expect(mobilePlayer.getByRole("slider")).toHaveCount(0);
  });

  test("play button toggles to pause via Alpine state", async ({ page }) => {
    const playerEl = page.locator("[x-data*=navbar-player]");
    await playerEl.evaluate((el) => {
      const state = (window as any).Alpine.$data(el);
      state.isPlaying = true;
    });

    const mobilePlayer = page.locator(".fixed.bottom-0");
    await expect(mobilePlayer.getByText("[ PAUSE ]")).toBeVisible();
  });
});
