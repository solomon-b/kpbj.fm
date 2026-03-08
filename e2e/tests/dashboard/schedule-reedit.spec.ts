import { test, expect } from "@playwright/test";
import { ADMIN_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Schedule Re-Edit tests
//
// Verifies that when a pending schedule exists and staff submits another
// schedule change, the old pending is cancelled and replaced by the new one.
//
// Scenario (sunday-morning-jazz, active schedule: Sunday 12AM-2AM weekly):
//   1. Change duration to 1hr (Sunday 12AM-1AM), starting 2027-06-01
//   2. Verify preview shows CURRENT + UPCOMING schedules
//   3. Change duration to 30min (Sunday 12AM-12:30AM), starting 2027-07-01
//   4. Verify old pending (June) replaced by new pending (July)
//
// Uses fetch() to POST form data directly, bypassing Alpine/HTMX form
// serialization which has issues with x-bind:value hidden inputs in nested
// Alpine scopes submitted through validateAndSubmit + htmx.ajax().
// ---------------------------------------------------------------------------

const SHOW_SLUG = "sunday-morning-jazz";
const EDIT_URL = `/dashboard/shows/${SHOW_SLUG}/edit`;
const POST_URL = `/dashboard/shows/${SHOW_SLUG}/edit`;

test.use({ storageState: ADMIN_AUTH });

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/**
 * Read current form field values from the edit page, then POST a schedule
 * change directly via fetch(). This bypasses the Alpine/HTMX form
 * serialization while still exercising the full server-side logic.
 */
async function postScheduleChange(
  page: import("@playwright/test").Page,
  schedule: {
    dayOfWeek: string;
    weeksOfMonth: number[];
    startTime: string;
    duration: number;
  },
  startDate: string,
) {
  // Collect existing form values so the POST doesn't blank out other fields
  const title = await page.inputValue('input[name="title"]');
  const description = await page
    .locator('textarea[name="description"]')
    .inputValue()
    .catch(() => "");
  const status = await page.inputValue('select[name="status"]');

  // Collect currently-checked host IDs
  const hostIds: string[] = await page.$$eval(
    'input[name="hosts"]:checked',
    (inputs) => inputs.map((el) => (el as HTMLInputElement).value),
  );

  // Collect tags
  const tags = await page
    .inputValue('input[name="tags"]')
    .catch(() => "");

  const schedulesJson = JSON.stringify([schedule]);

  const result = await page.evaluate(
    async (args) => {
      const fd = new FormData();
      fd.append("title", args.title);
      fd.append("description", args.description);
      fd.append("status", args.status);
      fd.append("tags", args.tags);
      fd.append("schedules_json", args.schedulesJson);
      fd.append("schedule_start_date", args.startDate);
      for (const id of args.hostIds) {
        fd.append("hosts", id);
      }

      const resp = await fetch(args.postUrl, {
        method: "POST",
        body: fd,
      });

      return {
        status: resp.status,
        hxRedirect: resp.headers.get("HX-Redirect"),
        body: await resp.text(),
      };
    },
    { title, description, status, tags, schedulesJson, startDate, hostIds, postUrl: POST_URL },
  );

  // The server always returns 200 (even for validation errors), so check
  // the HX-Redirect header to distinguish success from error.
  const redirect = result.hxRedirect ?? "";
  const bodyPreview = result.body.slice(0, 300);

  expect(
    redirect,
    `Expected HX-Redirect with success banner but got: HX-Redirect="${redirect}", body="${bodyPreview}"`,
  ).toContain("_banner=success");
}

// ---------------------------------------------------------------------------
// Tests (serial -- each step depends on the previous)
// ---------------------------------------------------------------------------

test.describe("Schedule re-edit", () => {
  test.describe.serial("pending schedule replacement", () => {
    // Step 1: Change duration from 2hr to 1hr, set future start date
    test("create pending schedule via edit", async ({ page }) => {
      await page.goto(EDIT_URL);
      await expect(page.getByText("EDIT SHOW")).toBeVisible();

      await postScheduleChange(
        page,
        {
          dayOfWeek: "sunday",
          weeksOfMonth: [1, 2, 3, 4, 5],
          startTime: "00:00",
          duration: 60,
        },
        "2027-06-01",
      );
    });

    // Step 2: Verify pending schedule appears in preview
    test("edit page shows current and pending schedule", async ({ page }) => {
      await page.goto(EDIT_URL);

      await expect(page.getByText("CURRENT SCHEDULE")).toBeVisible();
      await expect(
        page.getByText(/UPCOMING SCHEDULE.*2027-06-01/),
      ).toBeVisible();
    });

    // Step 3: Change duration to 30min with a different start date
    test("re-edit replaces pending schedule", async ({ page }) => {
      await page.goto(EDIT_URL);
      await expect(page.getByText("EDIT SHOW")).toBeVisible();

      await postScheduleChange(
        page,
        {
          dayOfWeek: "sunday",
          weeksOfMonth: [1, 2, 3, 4, 5],
          startTime: "00:00",
          duration: 30,
        },
        "2027-07-01",
      );
    });

    // Step 4: Verify the old pending was replaced
    test("edit page shows replaced pending schedule", async ({ page }) => {
      await page.goto(EDIT_URL);

      // Active schedule should still be present
      await expect(page.getByText("CURRENT SCHEDULE")).toBeVisible();

      // Pending should show the NEW date (July), not the old one (June)
      await expect(
        page.getByText(/UPCOMING SCHEDULE.*2027-07-01/),
      ).toBeVisible();

      // Old pending date should NOT appear
      await expect(
        page.getByText(/UPCOMING SCHEDULE.*2027-06-01/),
      ).not.toBeVisible();
    });
  });
});
