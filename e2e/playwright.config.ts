import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "./tests",
  outputDir: "./test-results",

  // Fail the build on CI if you accidentally left test.only in the source code.
  forbidOnly: !!process.env.CI,

  retries: process.env.CI ? 2 : 0,

  // Limit parallel workers in CI to avoid resource contention.
  workers: process.env.CI ? 1 : undefined,

  reporter: [["html", { outputFolder: "./playwright-report" }]],

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
