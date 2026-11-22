import { test, expect, type Page } from "@playwright/test";

// ---------------------------------------------------------------------------
// Login / Logout flow tests
//
// These tests do NOT use pre-authenticated storageState. They exercise the
// actual login mechanism: form submission, cookie setting, and redirects.
//
// IMPORTANT: Uses nightowl@example.com, which no other spec uses. A login
// as admin, staff, or host could invalidate the sessions of other specs,
// and users-crud.spec.ts suspends musicfan@example.com.
//
// API/User/Login/Post/HandlerSpec.hs covers each login result, so this
// file checks only what the browser does with them.
// ---------------------------------------------------------------------------

const EMAIL = "nightowl@example.com";

// Session cookie is environment-suffixed: "session-id-development".
async function sessionCookie(page: Page) {
  const cookies = await page.context().cookies();
  return cookies.find((c) => c.name === "session-id-development");
}

async function submitLogin(page: Page, password: string) {
  await page.goto("/user/login");
  await page.locator('input[name="email"]').fill(EMAIL);
  await page.locator('input[name="password"]').fill(password);
  await page.getByRole("button", { name: "LOGIN" }).click();
}

test.describe("Login", () => {
  test("login sets the session cookie and logout clears it", async ({ page }) => {
    await submitLogin(page, "password");
    await page.waitForURL("/");
    expect(await sessionCookie(page)).toBeDefined();

    await page.goto("/user/logout");
    await page.waitForURL("/");
    expect(await sessionCookie(page)).toBeUndefined();

    // With no session, /dashboard redirects to the login page.
    await page.goto("/dashboard");
    await page.waitForURL(/\/user\/login/);
  });

  test("failed login stays on the login page with the email pre-filled", async ({ page }) => {
    await submitLogin(page, "wrong-password");

    await page.waitForURL(/\/user\/login/);
    await expect(page.getByText("Your email address or password is invalid.")).toBeVisible();
    await expect(page.locator('input[name="email"]')).toHaveValue(EMAIL);
  });
});
