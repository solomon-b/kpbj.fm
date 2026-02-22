import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Login / Logout flow tests
//
// These tests do NOT use pre-authenticated storageState — they exercise the
// actual login mechanism: form submission, cookie setting, and redirects.
//
// IMPORTANT: Uses musicfan@example.com (not admin/staff/host) to avoid
// invalidating server-side sessions used by other test specs.
// ---------------------------------------------------------------------------

test.describe("Login", () => {
  test("login page loads with form fields", async ({ page }) => {
    await page.goto("/user/login");
    await expect(page.locator('input[name="email"]')).toBeVisible();
    await expect(page.locator('input[name="password"]')).toBeVisible();
    await expect(page.getByRole("button", { name: "LOGIN" })).toBeVisible();
  });

  test("successful login sets session cookie and redirects to /", async ({ page }) => {
    await page.goto("/user/login");
    await page.locator('input[name="email"]').fill("musicfan@example.com");
    await page.locator('input[name="password"]').fill("password");
    await page.getByRole("button", { name: "LOGIN" }).click();
    await page.waitForURL("/");

    // Session cookie is environment-suffixed: "session-id-development".
    const cookies = await page.context().cookies();
    const sessionCookie = cookies.find((c) => c.name === "session-id-development");
    expect(sessionCookie).toBeDefined();
  });

  test("failed login with wrong password stays on login page", async ({ page }) => {
    await page.goto("/user/login");
    await page.locator('input[name="email"]').fill("musicfan@example.com");
    await page.locator('input[name="password"]').fill("wrong-password");
    await page.getByRole("button", { name: "LOGIN" }).click();

    // Server redirects back to /user/login with email pre-filled.
    await page.waitForURL(/\/user\/login/);
    await expect(
      page.getByText("Your email address or password is invalid.")
    ).toBeVisible();

    // Email should be pre-filled.
    await expect(page.locator('input[name="email"]')).toHaveValue("musicfan@example.com");
  });

  test("failed login with unknown email stays on login page", async ({ page }) => {
    await page.goto("/user/login");
    await page.locator('input[name="email"]').fill("nobody@example.com");
    await page.locator('input[name="password"]').fill("password");
    await page.getByRole("button", { name: "LOGIN" }).click();

    await page.waitForURL(/\/user\/login/);
    await expect(
      page.getByText("Your email address or password is invalid.")
    ).toBeVisible();
  });
});

test.describe("Logout", () => {
  test("logout clears session and redirects to /", async ({ page }) => {
    // First, log in (using a user not shared with other specs).
    await page.goto("/user/login");
    await page.locator('input[name="email"]').fill("musicfan@example.com");
    await page.locator('input[name="password"]').fill("password");
    await page.getByRole("button", { name: "LOGIN" }).click();
    await page.waitForURL("/");

    // Now visit logout.
    await page.goto("/user/logout");
    await page.waitForURL("/");

    // After logout, accessing /dashboard should redirect to login.
    await page.goto("/dashboard");
    await page.waitForURL(/\/user\/login/);
  });
});

test.describe("Unauthenticated access", () => {
  test("/dashboard redirects to /user/login", async ({ page }) => {
    await page.goto("/dashboard");
    // The server returns a meta-refresh redirect to /user/login.
    await page.waitForURL(/\/user\/login/);
  });
});
