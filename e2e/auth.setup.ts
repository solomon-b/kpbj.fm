import { test as setup, type Page } from "@playwright/test";
import { ADMIN_AUTH, STAFF_AUTH, HOST_AUTH, USER_AUTH } from "./playwright.config";

// ---------------------------------------------------------------------------
// Auth setup — logs in as each role and saves storageState.
//
// Playwright runs this before any test in the "authenticated" project.
// Each test file then loads the appropriate storageState to start
// with a pre-authenticated session.
// ---------------------------------------------------------------------------

async function login(
  page: Page,
  email: string,
  password: string,
  storageStatePath: string,
) {
  await page.goto("/user/login");

  // Fill login form.
  await page.locator('input[name="email"]').fill(email);
  await page.locator('input[name="password"]').fill(password);

  // Submit — HTMX intercepts the POST. The server responds with an
  // HX-Redirect header, and HTMX navigates the browser to "/".
  await page.getByRole("button", { name: "LOGIN" }).click();
  await page.waitForURL("/");

  // Save cookies + localStorage for reuse by test files.
  await page.context().storageState({ path: storageStatePath });
}

setup("authenticate as admin", async ({ page }) => {
  await login(page, "admin@kpbj.fm", "password", ADMIN_AUTH);
});

setup("authenticate as staff", async ({ page }) => {
  await login(page, "staff@kpbj.fm", "password", STAFF_AUTH);
});

setup("authenticate as host", async ({ page }) => {
  await login(page, "host-sunday-morning-jazz@kpbj.fm", "password", HOST_AUTH);
});

setup("authenticate as user", async ({ page }) => {
  await login(page, "listener@example.com", "password", USER_AUTH);
});
