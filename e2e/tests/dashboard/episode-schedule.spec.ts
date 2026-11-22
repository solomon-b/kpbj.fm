import { test, expect, type Page } from "@playwright/test";
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
// Helpers
// ---------------------------------------------------------------------------

function unscheduledRow(page: Page) {
  return page.locator("#episodes-table-body tr", { hasText: UNSCHEDULED_DESC });
}

// Find the unscheduled episode's row and navigate to its edit page.
async function navigateToEdit(page: Page) {
  await page.goto(LIST_URL);
  await expect(unscheduledRow(page)).toBeVisible();
  await unscheduledRow(page).getByRole("link", { name: "Edit", exact: true }).click();
  await page.waitForURL(/\/dashboard\/episodes\/sunday-morning-jazz\/\d+\/edit/);
}

// Submit the edit form and wait for redirect back to the episode list.
// The HTMX form POSTs and returns HX-Redirect to the list page.
async function submitAndWaitForList(page: Page) {
  await page.getByRole("button", { name: "SAVE CHANGES" }).click();
  // Wait until we leave the /edit page — HTMX redirects to the list URL.
  await page.waitForURL((url) => !url.pathname.includes("/edit"));
  await expect(page.locator("#banner-container")).toContainText(/updated/i);
}

// The value of the slot option marked "(Current)".
async function currentSlot(page: Page): Promise<string> {
  const option = page.locator('select[name="scheduled_date"] option', { hasText: "(Current)" });
  await expect(option).toBeAttached();
  const value = await option.getAttribute("value");
  expect(value).toBeTruthy();
  return value!;
}

// The values of the slot options that can be picked.
async function slotOptions(page: Page): Promise<string[]> {
  return await page
    .locator("select[name='scheduled_date'] option:not([value='']):not([disabled])")
    .evaluateAll((els) => els.map((el) => el.getAttribute("value") ?? ""));
}

// ---------------------------------------------------------------------------
// Past episode schedule visibility (HOST)
// ---------------------------------------------------------------------------

test.describe("Past episode schedule visibility", () => {
  test.use({ storageState: HOST_AUTH });

  test("host cannot see schedule section for past episodes", async ({ page }) => {
    await page.goto(LIST_URL);
    // All mock episodes from 10_episodes.sql are in the past (past 3 weeks).
    // Pick a row that is NOT the unscheduled one.
    const pastRow = page
      .locator("#episodes-table-body tr")
      .filter({ hasNotText: UNSCHEDULED_DESC })
      .first();
    await expect(pastRow).toBeVisible();

    await pastRow.getByRole("link", { name: "Edit", exact: true }).click();
    await page.waitForURL(/\/dashboard\/episodes\/sunday-morning-jazz\/\d+\/edit/);

    // The schedule slot section is hidden for a past episode when the
    // user is a host (not staff).
    await expect(page.getByText("SCHEDULE SLOT")).not.toBeVisible();
  });
});

// ---------------------------------------------------------------------------
// Unscheduled display and schedule state transitions (ADMIN)
//
// One test, because each step changes the episode that the next step edits.
// ---------------------------------------------------------------------------

test.describe("Schedule state transitions", () => {
  test.use({ storageState: ADMIN_AUTH });

  test("unscheduled episode moves through the schedule states", async ({ page }) => {
    const select = page.locator('select[name="scheduled_date"]');

    await test.step("list and detail show the episode as unscheduled", async () => {
      await page.goto(LIST_URL);
      await expect(unscheduledRow(page)).toContainText("UNSCHEDULED");
      await unscheduledRow(page).locator("td").first().click();
      await page.waitForURL(/\/dashboard\/episodes\/sunday-morning-jazz\/\d+/);
      await expect(page.locator("text=Scheduled: Unscheduled")).toBeVisible();
    });

    await test.step("keep unscheduled episode unscheduled on edit", async () => {
      await navigateToEdit(page);
      await select.selectOption({ label: "Unscheduled (Current)" });
      await submitAndWaitForList(page);
      await expect(unscheduledRow(page)).toContainText("UNSCHEDULED");
    });

    await test.step("assign schedule slot to unscheduled episode", async () => {
      await navigateToEdit(page);
      const [firstSlot] = await slotOptions(page);
      expect(firstSlot).toBeTruthy();
      await select.selectOption(firstSlot);
      await submitAndWaitForList(page);
      await expect(unscheduledRow(page)).not.toContainText("UNSCHEDULED");
    });

    await test.step("keep current slot on scheduled episode", async () => {
      await navigateToEdit(page);
      // The form defaults to the "Select..." placeholder.
      await select.selectOption(await currentSlot(page));
      await submitAndWaitForList(page);
      await expect(unscheduledRow(page)).not.toContainText("UNSCHEDULED");
    });

    await test.step("reschedule episode to different slot", async () => {
      await navigateToEdit(page);
      const current = await currentSlot(page);
      const other = (await slotOptions(page)).find((v) => v && v !== current);
      expect(other).toBeTruthy();
      await select.selectOption(other!);
      await submitAndWaitForList(page);
      await expect(unscheduledRow(page)).not.toContainText("UNSCHEDULED");
    });
  });
});
