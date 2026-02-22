import { test, expect } from "@playwright/test";
import { USER_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// User role — dashboard access denial tests
//
// Users with the "User" role should be redirected away from all dashboard
// routes. The server returns NotAuthorized with role=User, which maps to
// a meta-refresh redirect to "/" (the public homepage).
// ---------------------------------------------------------------------------

test.use({ storageState: USER_AUTH });

test.describe("User role — dashboard access denied", () => {
  test("/dashboard/episodes redirects to /", async ({ page }) => {
    await page.goto("/dashboard/episodes");
    await page.waitForURL((url) => url.pathname === "/");
  });

  test("/dashboard/users redirects to /", async ({ page }) => {
    await page.goto("/dashboard/users");
    await page.waitForURL((url) => url.pathname === "/");
  });

  test("/dashboard/shows redirects to /", async ({ page }) => {
    await page.goto("/dashboard/shows");
    await page.waitForURL((url) => url.pathname === "/");
  });

  test("/dashboard/stream-settings redirects to /", async ({ page }) => {
    await page.goto("/dashboard/stream-settings");
    await page.waitForURL((url) => url.pathname === "/");
  });
});
