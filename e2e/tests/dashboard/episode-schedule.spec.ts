import { test, expect } from "@playwright/test";
import { ADMIN_AUTH, HOST_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Episode Schedule State Transition tests
//
// Covers the schedule section behaviour on the episode edit form:
// - Unscheduled episodes (NULL schedule_template_id / scheduled_at)
// - Scheduled episodes (has a schedule slot)
// - State transitions: keep unscheduled, assign slot, keep slot, reschedule
// - Host visibility: past episodes hide the schedule section
//
// Depends on mock-data/10a_unscheduled_episodes.sql inserting one unscheduled
// episode for sunday-morning-jazz with description
// "Unscheduled jazz episode for e2e testing".
//
// Form builder note: the select field always renders a disabled "Select..."
// placeholder as the first <option>. Template options added via addOption are
// NOT pre-selected, so the placeholder is the default selected option.
// ---------------------------------------------------------------------------

const SHOW_SLUG = "sunday-morning-jazz";
const LIST_URL = `/dashboard/episodes/${SHOW_SLUG}`;
const UNSCHEDULED_DESC = "Unscheduled jazz episode for e2e testing";

// ---------------------------------------------------------------------------
// Helper: find a row by description text and navigate to its edit page.
// ---------------------------------------------------------------------------
async function navigateToEditByDescription(
  page: import("@playwright/test").Page,
  description: string,
) {
  await page.goto(LIST_URL);
  const row = page.locator("#episodes-table-body tr", {
    hasText: description,
  });
  await expect(row).toBeVisible();
  // Use the actions dropdown to navigate to edit
  await row.locator("select").selectOption("edit");
  await page.waitForURL(/\/dashboard\/episodes\/sunday-morning-jazz\/\d+\/edit/);
}

// Helper: submit the edit form and wait for redirect back to the episode list.
// The HTMX form POSTs and returns HX-Redirect to the list page.
async function submitAndWaitForList(
  page: import("@playwright/test").Page,
) {
  await page.getByRole("button", { name: "SAVE CHANGES" }).click();
  // Wait until we leave the /edit page — HTMX redirects to the list URL.
  await page.waitForURL((url) => !url.pathname.includes("/edit"));
  await expect(page.locator("#banner-container")).toContainText(/updated/i);
}

// ---------------------------------------------------------------------------
// Unscheduled episode display (ADMIN)
// ---------------------------------------------------------------------------

test.describe("Unscheduled episode display", () => {
  test.use({ storageState: ADMIN_AUTH });

  test("unscheduled episode shows UNSCHEDULED badge in list", async ({
    page,
  }) => {
    await page.goto(LIST_URL);
    const row = page.locator("#episodes-table-body tr", {
      hasText: UNSCHEDULED_DESC,
    });
    await expect(row).toBeVisible();
    await expect(row).toContainText("UNSCHEDULED");
  });

  test("unscheduled episode detail shows Scheduled: Unscheduled", async ({
    page,
  }) => {
    await page.goto(LIST_URL);
    const row = page.locator("#episodes-table-body tr", {
      hasText: UNSCHEDULED_DESC,
    });
    await expect(row).toBeVisible();
    // Click the row to navigate to detail page
    await row.locator("td").first().click();
    await page.waitForURL(
      /\/dashboard\/episodes\/sunday-morning-jazz\/\d+/,
    );
    // Use locator scoped to the metadata grid to avoid matching the description
    await expect(
      page.locator("text=Scheduled: Unscheduled"),
    ).toBeVisible();
  });
});

// ---------------------------------------------------------------------------
// Past episode schedule visibility (HOST)
// ---------------------------------------------------------------------------

test.describe("Past episode schedule visibility", () => {
  test.use({ storageState: HOST_AUTH });

  test("host cannot see schedule section for past episodes", async ({
    page,
  }) => {
    await page.goto(LIST_URL);
    // All mock episodes from 10_episodes.sql are in the past (past 3 weeks).
    // Find a row that is NOT the unscheduled one (those are the scheduled past episodes).
    const rows = page.locator("#episodes-table-body tr");
    await expect(rows.first()).toBeVisible();

    // Pick a past scheduled episode (any row not containing the unscheduled description)
    const pastRow = page
      .locator("#episodes-table-body tr")
      .filter({ hasNotText: UNSCHEDULED_DESC })
      .first();
    await expect(pastRow).toBeVisible();

    // Navigate to its edit page via actions dropdown
    await pastRow.locator("select").selectOption("edit");
    await page.waitForURL(
      /\/dashboard\/episodes\/sunday-morning-jazz\/\d+\/edit/,
    );

    // The schedule slot section should NOT be visible for a past episode
    // when the user is a host (not staff).
    // The section title "SCHEDULE SLOT" should not appear.
    await expect(page.getByText("SCHEDULE SLOT")).not.toBeVisible();
  });
});

// ---------------------------------------------------------------------------
// Schedule state transitions (ADMIN, serial)
//
// These tests are serial because T3 changes the episode from
// unscheduled → scheduled, which T4 and T5 depend on.
//
// The form builder's select field has a disabled "Select..." placeholder
// that is selected by default. The template's addOption calls are NOT
// pre-selected, so we must explicitly select the desired option.
// ---------------------------------------------------------------------------

test.describe("Schedule state transitions", () => {
  test.use({ storageState: ADMIN_AUTH });

  test.describe.serial("schedule transitions", () => {
    // T2: Keep unscheduled episode unscheduled on edit
    test("keep unscheduled episode unscheduled on edit", async ({ page }) => {
      await navigateToEditByDescription(page, UNSCHEDULED_DESC);

      // The schedule select should be visible
      const select = page.locator('select[name="scheduled_date"]');
      await expect(select).toBeVisible();

      // The "Unscheduled (Current)" option should exist
      const unschedOption = select.locator("option", {
        hasText: "Unscheduled (Current)",
      });
      await expect(unschedOption).toBeAttached();

      // Explicitly select the "Unscheduled" option (value "") to keep it unscheduled
      await select.selectOption({ label: "Unscheduled (Current)" });

      // Submit and wait for redirect
      await submitAndWaitForList(page);

      // Verify episode is still unscheduled in the list
      const row = page.locator("#episodes-table-body tr", {
        hasText: UNSCHEDULED_DESC,
      });
      await expect(row).toContainText("UNSCHEDULED");
    });

    // T3: Assign schedule slot to unscheduled episode
    test("assign schedule slot to unscheduled episode", async ({ page }) => {
      await navigateToEditByDescription(page, UNSCHEDULED_DESC);

      const select = page.locator('select[name="scheduled_date"]');
      await expect(select).toBeVisible();

      // Pick the first available (non-empty, non-disabled) schedule slot
      const firstOption = select.locator(
        "option:not([value='']):not([disabled])",
      ).first();
      await expect(firstOption).toBeAttached();
      const slotValue = await firstOption.getAttribute("value");
      expect(slotValue).toBeTruthy();
      await select.selectOption(slotValue!);

      // Submit and wait for redirect
      await submitAndWaitForList(page);

      // Verify episode is no longer unscheduled in the list
      const row = page.locator("#episodes-table-body tr", {
        hasText: UNSCHEDULED_DESC,
      });
      await expect(row).toBeVisible();
      await expect(row).not.toContainText("UNSCHEDULED");
    });

    // T4: Keep current slot on scheduled episode
    test("keep current slot on scheduled episode", async ({ page }) => {
      await navigateToEditByDescription(page, UNSCHEDULED_DESC);

      const select = page.locator('select[name="scheduled_date"]');
      await expect(select).toBeVisible();

      // Find the "(Current)" option — it's the current schedule slot
      const currentOption = select.locator("option", {
        hasText: "(Current)",
      });
      await expect(currentOption).toBeAttached();
      const currentValue = await currentOption.getAttribute("value");
      expect(currentValue).toBeTruthy();

      // Explicitly select the current slot (form defaults to "Select..." placeholder)
      await select.selectOption(currentValue!);

      // Submit and wait for redirect
      await submitAndWaitForList(page);

      // Verify episode is still scheduled (no UNSCHEDULED badge)
      const row = page.locator("#episodes-table-body tr", {
        hasText: UNSCHEDULED_DESC,
      });
      await expect(row).toBeVisible();
      await expect(row).not.toContainText("UNSCHEDULED");
    });

    // T5: Reschedule episode to different slot
    test("reschedule episode to different slot", async ({ page }) => {
      await navigateToEditByDescription(page, UNSCHEDULED_DESC);

      const select = page.locator('select[name="scheduled_date"]');
      await expect(select).toBeVisible();

      // Find the current slot value
      const currentOption = select.locator("option", {
        hasText: "(Current)",
      });
      await expect(currentOption).toBeAttached();
      const currentValue = await currentOption.getAttribute("value");

      // Find a different slot (non-empty, non-disabled, not the current one)
      const allSlotOptions = select.locator(
        "option:not([value='']):not([disabled])",
      );
      const optionCount = await allSlotOptions.count();
      // Need at least 2 options (current + at least one alternative)
      expect(optionCount).toBeGreaterThanOrEqual(2);

      // Pick the first option whose value differs from the current slot
      let altValue: string | null = null;
      for (let i = 0; i < optionCount; i++) {
        const val = await allSlotOptions.nth(i).getAttribute("value");
        if (val && val !== currentValue) {
          altValue = val;
          break;
        }
      }
      expect(altValue).toBeTruthy();
      await select.selectOption(altValue!);

      // Submit and wait for redirect
      await submitAndWaitForList(page);

      // Verify episode is still scheduled (no UNSCHEDULED badge)
      const row = page.locator("#episodes-table-body tr", {
        hasText: UNSCHEDULED_DESC,
      });
      await expect(row).toBeVisible();
      await expect(row).not.toContainText("UNSCHEDULED");
    });
  });
});
