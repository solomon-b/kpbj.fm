import { test } from "@playwright/test";
import { USER_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// User role — dashboard access denial test
//
// App/Auth/GateSpec.hs covers the gate for each role and route kind. This
// test checks that the browser follows the redirect: a User who opens a
// dashboard page lands on "/" (the public homepage).
// ---------------------------------------------------------------------------

test.use({ storageState: USER_AUTH });

test.describe("User role — dashboard access denied", () => {
  test("a dashboard page redirects to /", async ({ page }) => {
    await page.goto("/dashboard/users");
    await page.waitForURL((url) => url.pathname === "/");
  });
});
