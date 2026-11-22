import { test, expect } from "@playwright/test";
import { CROSS_BROWSER } from "../playwright.config";

// ---------------------------------------------------------------------------
// Homepage tests
// ---------------------------------------------------------------------------
// The homepage (GET /) has three main areas:
//   1. Featured event image (or fallback range map image)
//   2. Newsletter signup form ("Stay in the Loop")
//   3. The frame: nav, player, footer (shared across all pages)
//
// Desktop layout only. See mobile.spec.ts for the mobile player.

const FEATURED_IMAGE = 'img[alt="Spring Record Fair & Swap Meet event flyer"]';

test.describe("Homepage", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/");
  });

  // -------------------------------------------------------------------------
  // Page content
  // -------------------------------------------------------------------------

  test("renders the frame, featured event, and newsletter form", async ({ page }) => {
    await expect(page).toHaveTitle("KPBJ 95.9FM");

    // #main-content is the HTMX swap target. All page content lives here.
    await expect(page.locator("#main-content")).toBeVisible();
    await expect(page.getByText("Sun Valley Arts and Culture")).toBeVisible();

    // Mock data has "Spring Record Fair & Swap Meet" with
    // featured_on_homepage = TRUE.
    await expect(page.locator(FEATURED_IMAGE)).toBeVisible();

    await expect(page.getByRole("heading", { name: "Stay in the Loop" })).toBeVisible();
    await expect(page.getByRole("textbox", { name: "Email Address" })).toBeVisible();
    await expect(page.getByRole("button", { name: "Subscribe" })).toBeVisible();
  });

  test("clicking featured event navigates to event detail", async ({ page }) => {
    await page.locator(`a:has(${FEATURED_IMAGE})`).click();
    await expect(page).toHaveURL(/\/events\/\d+\/spring-record-fair/);
  });

  // -------------------------------------------------------------------------
  // Newsletter signup
  // -------------------------------------------------------------------------

  test("newsletter form shows confirmation on submit", { tag: CROSS_BROWSER }, async ({ page }) => {
    // Use a unique email per run so retries don't collide with the unique
    // constraint (duplicate inserts silently no-op, but the user-visible
    // outcome is identical, so we still assert the same swap).
    const email = `e2e+${Date.now()}@example.com`;

    await page.getByRole("textbox", { name: "Email Address" }).fill(email);
    await page.getByRole("button", { name: "Subscribe" }).click();

    // HTMX swaps the form's wrapper (#newsletter-signup) with a thanks
    // fragment whose root div carries the same id and renders the text.
    await expect(page.locator("#newsletter-signup")).toHaveText("Thanks for subscribing!");
    await expect(page.getByRole("button", { name: "Subscribe" })).toHaveCount(0);
  });

  // -------------------------------------------------------------------------
  // Stream player
  // -------------------------------------------------------------------------
  // The player is an Alpine.js component wrapping an <audio> element.
  // Icecast isn't running in dev, so the test checks the UI without
  // audio playback.

  test("player shows now playing and its volume slider sets the audio volume", { tag: CROSS_BROWSER }, async ({ page }) => {
    // Without Icecast the metadata handler returns the station name.
    await expect(page.getByText("KPBJ 95.9 FM").locator("visible=true").first()).toBeVisible();

    // The desktop play button's text is set by Alpine: '[ PLAY ]' or '[ PAUSE ]'.
    await expect(page.getByText("[ PLAY ]").locator("visible=true")).toBeVisible();
    await expect(page.getByText("VOL:")).toBeVisible();

    // The slider is 0-100. The audio element volume is 0-1.
    const audio = page.locator("audio");
    await expect(audio).toBeAttached();
    await page.getByRole("slider").locator("visible=true").fill("30");
    const volume = await audio.evaluate((el: HTMLAudioElement) => el.volume);
    expect(volume).toBeCloseTo(0.3, 1);
  });
});
