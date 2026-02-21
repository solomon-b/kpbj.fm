import { defineConfig, devices } from "@playwright/test";

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
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },
    {
      name: "firefox",
      use: { ...devices["Desktop Firefox"] },
    },
    {
      // iPhone 13 viewport + touch. Uses Chromium instead of WebKit
      // because WebKit crashes on NixOS (missing WPE EGL display).
      name: "mobile",
      use: {
        ...devices["iPhone 13"],
        defaultBrowserType: "chromium",
      },
    },
    {
      name: "mobile-android",
      use: { ...devices["Pixel 5"] },
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
