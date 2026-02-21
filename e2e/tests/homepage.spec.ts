import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Homepage tests
// ---------------------------------------------------------------------------
// The homepage (GET /) has three main areas:
//   1. Featured event image (or fallback range map image)
//   2. Newsletter signup form ("Stay in the Loop")
//   3. The frame — nav, player, footer (shared across all pages)
//
// We test each section independently.

test.describe("Homepage", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/");
  });

  // -------------------------------------------------------------------------
  // Page structure
  // -------------------------------------------------------------------------

  test("has the correct title", async ({ page }) => {
    await expect(page).toHaveTitle("KPBJ 95.9FM");
  });

  test("has a main content area", async ({ page }) => {
    // #main-content is the HTMX swap target — all page content lives here.
    await expect(page.locator("#main-content")).toBeVisible();
  });

  test("has a footer with copyright", async ({ page }) => {
    // getByText finds any element containing this text.
    // The footer has: "© 2025 Sun Valley Arts and Culture..."
    await expect(page.getByText("Sun Valley Arts and Culture")).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Navigation links
  // -------------------------------------------------------------------------
  // Each nav link has a stable id like id="nav-shows". We verify they all
  // exist and are clickable. We use the desktop nav (visible at default
  // viewport width).

  test("has all navigation links", async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.startsWith("mobile"), "Desktop nav — see mobile.spec.ts");
    const navLinks = ["Shows", "Schedule", "Donate", "Events", "About"];

    for (const linkText of navLinks) {
      // getByRole("link") matches <a> tags. { name } matches visible text.
      // We use .locator("visible=true") because mobile + desktop navs
      // both have links with the same text.
      const link = page.getByRole("link", { name: linkText }).locator("visible=true");
      await expect(link).toBeVisible();
    }
  });

  // -------------------------------------------------------------------------
  // Featured image
  // -------------------------------------------------------------------------
  // The homepage shows either a featured event poster or a fallback image
  // (the FCC range map at /static/range.png). One of them will always be
  // present inside #main-content.

  test("displays a featured image or fallback", async ({ page }) => {
    // Look for any <img> inside the main content area.
    const image = page.locator("#main-content img");
    await expect(image.first()).toBeVisible();
  });

  test("displays the featured event image with alt text", async ({ page }) => {
    // Mock data has "Spring Record Fair & Swap Meet" with featured_on_homepage = TRUE.
    // The image alt text is "{title} event flyer".
    await expect(
      page.locator('img[alt="Spring Record Fair & Swap Meet event flyer"]')
    ).toBeVisible();
  });

  test("featured event image links to the event detail page", async ({ page }) => {
    const eventLink = page.locator('a:has(img[alt="Spring Record Fair & Swap Meet event flyer"])');
    const href = await eventLink.getAttribute("href");
    expect(href).toMatch(/\/events\/\d+\/spring-record-fair/);
  });

  test("clicking featured event navigates to event detail", async ({ page }) => {
    const eventLink = page.locator('a:has(img[alt="Spring Record Fair & Swap Meet event flyer"])');
    await eventLink.click();
    await expect(page).toHaveURL(/\/events\/\d+\/spring-record-fair/);
  });

  // -------------------------------------------------------------------------
  // Newsletter signup
  // -------------------------------------------------------------------------

  test("shows the newsletter heading", async ({ page }) => {
    await expect(page.getByRole("heading", { name: "Stay in the Loop" })).toBeVisible();
  });

  test("has an email input and subscribe button", async ({ page }) => {
    // getByRole("textbox") matches <input> elements.
    // { name: "Email Address" } matches the aria-label on the input.
    await expect(page.getByRole("textbox", { name: "Email Address" })).toBeVisible();
    await expect(page.getByRole("button", { name: "Subscribe" })).toBeVisible();
  });

  test("newsletter form shows confirmation on submit", async ({ page }) => {
    // The form POSTs to a real Google Forms endpoint (even in dev), so we
    // intercept the request to prevent actual submissions. page.route()
    // matches outgoing requests by URL pattern and lets you handle them.
    await page.route("**/docs.google.com/forms/**", (route) => {
      // fulfill() returns a fake response without hitting the real server.
      route.fulfill({ status: 200, body: "" });
    });

    // Fill in a test email address.
    await page.getByRole("textbox", { name: "Email Address" }).fill("test@example.com");

    // Click Subscribe.
    await page.getByRole("button", { name: "Subscribe" }).click();

    // The confirmation message is hidden by default (display: none) and
    // gets shown by the handleSubmit() JavaScript after a 500ms delay.
    // Playwright's toBeVisible() will wait for it to appear.
    await expect(page.locator("#form-confirmation")).toBeVisible();

    // Verify the confirmation text.
    await expect(page.locator("#form-confirmation")).toHaveText("Thanks for subscribing!");
  });

  // -------------------------------------------------------------------------
  // Stream player
  // -------------------------------------------------------------------------
  // The player is an Alpine.js component wrapping an <audio> element.
  // Icecast isn't running in dev, so we mock the metadata endpoint and
  // test UI behavior without actual audio playback.

  test("player has play button and volume slider", async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.startsWith("mobile"), "No volume slider on mobile player");
    // The desktop play button's text is set by Alpine: '[ PLAY ]' or '[ PAUSE ]'.
    // getByText with exact: true avoids matching substrings.
    await expect(page.getByText("[ PLAY ]").locator("visible=true")).toBeVisible();

    // The volume slider is an <input type="range">.
    // getByRole("slider") matches range inputs.
    await expect(page.getByRole("slider").locator("visible=true")).toBeVisible();

    // Volume label.
    await expect(page.getByText("VOL:")).toBeVisible();
  });

  test("player shows now-playing text", async ({ page }) => {
    // On init, Alpine calls fetchMetadata(). Even without Icecast, the
    // handler returns a fallback with title "KPBJ 95.9 FM". Since
    // currentShow is set to that (truthy), the x-text || fallback
    // "NOW PLAYING: ..." never kicks in. The visible text is just the title.
    await expect(page.getByText("KPBJ 95.9 FM").locator("visible=true").first()).toBeVisible();
  });

  test("play button toggles to pause via Alpine state", async ({ page }) => {
    // Actually playing audio requires a running Icecast server, so instead
    // we test the UI reactivity by setting Alpine state directly.
    //
    // page.evaluate() runs JavaScript in the browser. Here we:
    //   1. Find the Alpine component (the parent div with x-data)
    //   2. Set isPlaying = true (simulating successful playback)
    //   3. Alpine reactively updates the button text
    const playerEl = page.locator("[x-data*=navbar-player]");
    await playerEl.evaluate((el) => {
      // Alpine.$data() gives us the reactive proxy for the component.
      const state = (window as any).Alpine.$data(el);
      state.isPlaying = true;
    });

    // After setting state, Alpine updates the button text to '[ PAUSE ]'.
    await expect(page.getByText("[ PAUSE ]").locator("visible=true")).toBeVisible();
  });

  test("volume slider controls audio element", async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.startsWith("mobile"), "No volume slider on mobile player");
    // page.evaluate() runs JavaScript directly in the browser.
    // We use it to read the Alpine state from the audio element.
    const audio = page.locator("audio");

    // The audio element should exist (even if not playing).
    await expect(audio).toBeAttached();

    // Fill the range input to a new value. Playwright's fill() works on
    // range inputs — it sets the value directly.
    await page.getByRole("slider").locator("visible=true").fill("30");

    // Verify the audio element's volume updated.
    // evaluate() runs a function in the browser context, receiving the
    // DOM element as its argument.
    const volume = await audio.evaluate((el: HTMLAudioElement) => el.volume);

    // Volume is 0-1 in the DOM, 0-100 on the slider.
    expect(volume).toBeCloseTo(0.3, 1);
  });

  test("metadata endpoint returns valid response", async ({ page }) => {
    // page.request is Playwright's built-in HTTP client — it can make
    // requests directly without going through the browser.
    // Useful for testing API endpoints.
    const response = await page.request.get("/api/stream/metadata");
    expect(response.status()).toBe(200);

    const json = await response.json();
    // Even when Icecast is down, the handler returns a fallback structure.
    expect(json).toHaveProperty("icestats");
    expect(json.icestats).toHaveProperty("source");
  });
});
