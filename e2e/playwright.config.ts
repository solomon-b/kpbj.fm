import { defineConfig, devices } from "@playwright/test";
import path from "path";

// Auth storage state file paths. Test files import these.
export const ADMIN_AUTH = path.join(__dirname, ".auth", "admin.json");
export const STAFF_AUTH = path.join(__dirname, ".auth", "staff.json");
export const HOST_AUTH = path.join(__dirname, ".auth", "host.json");
export const USER_AUTH = path.join(__dirname, ".auth", "user.json");

// The tag of the public tests that also run in Firefox. Give it to a test
// that depends on browser behavior: Alpine or HTMX code, localStorage,
// history, scrolling, or keyboard events. Server-rendered content gives the
// same result in each engine, so Chromium alone checks it.
export const CROSS_BROWSER = "@cross-browser";

// The public specs that run at desktop width. mobile.spec.ts holds the
// tests for the mobile layout.
const desktopOnly = [/dashboard\//, /mobile\.spec\.ts/];

export default defineConfig({
  testDir: "./tests",
  outputDir: "./test-results",

  // Fail the build on CI if you accidentally left test.only in the source code.
  forbidOnly: !!process.env.CI,

  // Retry once. This handles transient navigation timeouts when the
  // parallel workers contend for a single dev server.
  retries: 1,

  // GitHub Actions runners for private repos have 2 vCPUs. Use both.
  workers: process.env.CI ? 2 : undefined,

  reporter: process.env.CI
    ? [["html", { outputFolder: "./playwright-report" }], ["list"]]
    : [["html", { outputFolder: "./playwright-report" }]],

  use: {
    baseURL: "http://localhost:4000",

    // Clicks and fills must be fast, so the action timeout is tight. The
    // navigation timeout is generous because the workers share one server.
    actionTimeout: 5_000,
    navigationTimeout: 20_000,

    // Collect trace on first retry for debugging CI failures.
    trace: "on-first-retry",
  },

  projects: [
    // --- Auth setup (runs first, before authenticated tests) ---
    {
      name: "setup",
      testDir: ".",
      testMatch: "auth.setup.ts",
    },

    // --- Public site tests ---
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
      testIgnore: desktopOnly,
    },
    {
      // Only the tests tagged CROSS_BROWSER.
      name: "firefox",
      use: { ...devices["Desktop Firefox"] },
      testIgnore: desktopOnly,
      grep: new RegExp(CROSS_BROWSER),
    },
    {
      // iPhone 13 viewport + touch. Uses Chromium instead of WebKit
      // because WebKit crashes on NixOS (missing WPE EGL display).
      name: "mobile",
      use: {
        ...devices["iPhone 13"],
        defaultBrowserType: "chromium",
      },
      testMatch: /mobile\.spec\.ts/,
    },

    // --- Dashboard tests (require auth setup) ---
    {
      name: "authenticated",
      testMatch: /dashboard\/.+\.spec\.ts/,
      use: { ...devices["Desktop Chrome"] },
      dependencies: ["setup"],
    },
  ],

  // Start the web server before running tests.
  // Disabled by default. Enable with USE_WEB_SERVER=1, or
  // start the server yourself with `just run` before running tests.
  // CI starts the server explicitly in the workflow.
  ...(process.env.USE_WEB_SERVER
    ? {
        webServer: {
          command: "just run",
          url: "http://localhost:4000",
          reuseExistingServer: !process.env.CI,
          timeout: 120_000,
        },
      }
    : {}),
});
