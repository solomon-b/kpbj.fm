import { test, expect } from "@playwright/test";
import { HOST_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Profile CRUD tests
//
// Covers loading the profile form, editing display name, changing theme,
// and sidebar navigation. Uses HOST_AUTH (Smooth Sullivan).
//
// Edit tests change values and then restore originals so mock data stays
// consistent for other test suites.
// ---------------------------------------------------------------------------

test.use({ storageState: HOST_AUTH });

test.describe("Profile CRUD", () => {
  // -------------------------------------------------------------------------
  // Load form
  // -------------------------------------------------------------------------

  test("profile edit form loads with pre-filled fields", async ({
    page,
  }) => {
    await page.goto("/dashboard/profile/edit");

    // Email is displayed but disabled.
    const emailInput = page.locator('input[name="email"]');
    await expect(emailInput).toBeVisible();
    await expect(emailInput).toBeDisabled();
    await expect(emailInput).toHaveValue(
      "host-sunday-morning-jazz@kpbj.fm"
    );

    // Display name is pre-filled.
    await expect(
      page.locator('input[name="display_name"]')
    ).toHaveValue("Smooth Sullivan");

    // Full name is pre-filled.
    const fullNameInput = page.locator('input[name="full_name"]');
    await expect(fullNameInput).toBeVisible();
    const fullNameValue = await fullNameInput.inputValue();
    expect(fullNameValue.length).toBeGreaterThan(0);
  });

  test("color scheme and theme radios are visible", async ({ page }) => {
    await page.goto("/dashboard/profile/edit");

    // Color scheme radios.
    await expect(
      page.locator('input[name="color_scheme"][value="Automatic"]')
    ).toBeAttached();
    await expect(
      page.locator('input[name="color_scheme"][value="LightMode"]')
    ).toBeAttached();
    await expect(
      page.locator('input[name="color_scheme"][value="DarkMode"]')
    ).toBeAttached();

    // Theme radios.
    await expect(
      page.locator('input[name="theme"][value="Default"]')
    ).toBeAttached();
    await expect(
      page.locator('input[name="theme"][value="Solarized"]')
    ).toBeAttached();
    await expect(
      page.locator('input[name="theme"][value="Gruvbox"]')
    ).toBeAttached();
    await expect(
      page.locator('input[name="theme"][value="Dracula"]')
    ).toBeAttached();
    await expect(
      page.locator('input[name="theme"][value="Nord"]')
    ).toBeAttached();
  });

  // -------------------------------------------------------------------------
  // Edit display name (and restore)
  // -------------------------------------------------------------------------

  test.describe.serial("edit display name", () => {
    test("change display name and save", async ({ page }) => {
      await page.goto("/dashboard/profile/edit");
      const displayName = page.locator('input[name="display_name"]');
      await displayName.fill("Smooth Sullivan E2E");
      await page.getByRole("button", { name: "SAVE CHANGES" }).click();

      // Should redirect with success banner.
      await expect(page.locator("#banner-container")).toContainText(
        /updated|Updated|saved|Saved/i
      );
    });

    test("verify changed name and restore original", async ({ page }) => {
      await page.goto("/dashboard/profile/edit");
      await expect(
        page.locator('input[name="display_name"]')
      ).toHaveValue("Smooth Sullivan E2E");

      // Restore original.
      await page
        .locator('input[name="display_name"]')
        .fill("Smooth Sullivan");
      await page.getByRole("button", { name: "SAVE CHANGES" }).click();
      await expect(page.locator("#banner-container")).toContainText(
        /updated|Updated|saved|Saved/i
      );
    });
  });

  // -------------------------------------------------------------------------
  // Change theme (and restore)
  // -------------------------------------------------------------------------

  test.describe.serial("change theme", () => {
    test("select Gruvbox theme and save", async ({ page }) => {
      await page.goto("/dashboard/profile/edit");
      await page
        .locator('input[name="theme"][value="Gruvbox"]')
        .check();
      await page.getByRole("button", { name: "SAVE CHANGES" }).click();
      await expect(page.locator("#banner-container")).toContainText(
        /updated|Updated|saved|Saved/i
      );
    });

    test("verify Gruvbox is selected and restore Default", async ({
      page,
    }) => {
      await page.goto("/dashboard/profile/edit");
      await expect(
        page.locator('input[name="theme"][value="Gruvbox"]')
      ).toBeChecked();

      // Restore default theme.
      await page
        .locator('input[name="theme"][value="Default"]')
        .check();
      await page.getByRole("button", { name: "SAVE CHANGES" }).click();
      await expect(page.locator("#banner-container")).toContainText(
        /updated|Updated|saved|Saved/i
      );
    });
  });

  // -------------------------------------------------------------------------
  // Navigation
  // -------------------------------------------------------------------------

  test("sidebar Settings link navigates to profile edit", async ({
    page,
  }) => {
    // Start on a dashboard page.
    await page.goto("/dashboard/episodes/sunday-morning-jazz");
    await page
      .locator("aside")
      .getByRole("link", { name: "Settings", exact: true })
      .click();
    await page.waitForURL(/\/dashboard\/profile\/edit/);
    await expect(page.locator('input[name="email"]')).toBeVisible();
  });
});
