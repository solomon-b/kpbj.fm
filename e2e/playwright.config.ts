import { defineConfig, devices } from "@playwright/test";
import path from "path";

// Auth storage state file paths — imported by test files.
export const ADMIN_AUTH = path.join(__dirname, ".auth", "admin.json");
export const STAFF_AUTH = path.join(__dirname, ".auth", "staff.json");
export const HOST_AUTH = path.join(__dirname, ".auth", "host.json");
export const USER_AUTH = path.join(__dirname, ".auth", "user.json");

export default defineConfig({
  testDir: "./tests",
  outputDir: "./test-results",

  // Fail the build on CI if you accidentally left test.only in the source code.
  forbidOnly: !!process.env.CI,

  retries: process.env.CI ? 1 : 0,

  // GitHub Actions runners have 2 vCPUs — use both.
  workers: process.env.CI ? 2 : undefined,

  reporter: process.env.CI
    ? [["html", { outputFolder: "./playwright-report" }], ["list"]]
    : [["html", { outputFolder: "./playwright-report" }]],

  use: {
    baseURL: "http://localhost:4000",

    // Shorter timeouts — server-rendered HTML loads fast.
    actionTimeout: 5_000,
    navigationTimeout: 10_000,

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

    // --- Public site tests (skip dashboard specs) ---
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
      testIgnore: /dashboard\//,
    },
    {
      name: "firefox",
      use: { ...devices["Desktop Firefox"] },
      testIgnore: /dashboard\//,
    },
    {
      // iPhone 13 viewport + touch. Uses Chromium instead of WebKit
      // because WebKit crashes on NixOS (missing WPE EGL display).
      name: "mobile",
      use: {
        ...devices["iPhone 13"],
        defaultBrowserType: "chromium",
      },
      testIgnore: /dashboard\//,
    },
    {
      name: "mobile-android",
      use: { ...devices["Pixel 5"] },
      testIgnore: /dashboard\//,
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
  // Disabled by default — enable with USE_WEB_SERVER=1, or
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
