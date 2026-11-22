import { test, expect, type Page } from "@playwright/test";
import { HOST_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Profile CRUD tests
//
// Covers loading the profile form, editing display name and theme, and
// sidebar navigation. Uses HOST_AUTH (Smooth Sullivan).
//
// The edit test changes values and then restores the originals so mock data
// stays consistent for other test suites.
// ---------------------------------------------------------------------------

test.use({ storageState: HOST_AUTH });

async function save(page: Page) {
  await page.getByRole("button", { name: "SAVE CHANGES" }).click();
  await expect(page.locator("#banner-container")).toContainText(/updated|Updated|saved|Saved/i);
}

test.describe("Profile CRUD", () => {
  test("profile edit form loads with pre-filled fields", async ({ page }) => {
    await page.goto("/dashboard/profile/edit");

    // Email is displayed but disabled.
    const emailInput = page.locator('input[name="email"]');
    await expect(emailInput).toBeDisabled();
    await expect(emailInput).toHaveValue("host-sunday-morning-jazz@kpbj.fm");

    await expect(page.locator('input[name="display_name"]')).toHaveValue("Smooth Sullivan");
    await expect(page.locator('input[name="full_name"]')).toHaveValue(/\S/);
  });

  test("display name and theme save and restore", async ({ page }) => {
    const displayName = page.locator('input[name="display_name"]');
    const gruvbox = page.locator('input[name="theme"][value="Gruvbox"]');

    await test.step("change display name and theme", async () => {
      await page.goto("/dashboard/profile/edit");
      await displayName.fill("Smooth Sullivan E2E");
      await gruvbox.check();
      await save(page);
    });

    await test.step("verify the changes and restore the originals", async () => {
      await page.goto("/dashboard/profile/edit");
      await expect(displayName).toHaveValue("Smooth Sullivan E2E");
      await expect(gruvbox).toBeChecked();

      await displayName.fill("Smooth Sullivan");
      await page.locator('input[name="theme"][value="Default"]').check();
      await save(page);
    });
  });

  test("sidebar Settings link navigates to profile edit", async ({ page }) => {
    await page.goto("/dashboard/episodes/sunday-morning-jazz");
    await page.locator("aside").getByRole("link", { name: "Settings", exact: true }).click();
    await page.waitForURL(/\/dashboard\/profile\/edit/);
    await expect(page.locator('input[name="email"]')).toBeVisible();
  });
});
