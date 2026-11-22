import { test, expect, type Page } from "@playwright/test";
import { STAFF_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Invitations CRUD tests
//
// Covers list, create, edit, resend, revoke, and regenerate for
// host invitations. Uses STAFF_AUTH (only staff+ can manage invitations).
// Each test revokes what it creates, so the cleanup also checks revoke.
// host-onboarding.spec.ts reads invite links with the Copy Link button.
//
// The create form embeds an Alpine.js ScheduleEditor that's awkward to
// drive from Playwright — we follow the schedule-reedit.spec.ts pattern
// and POST a synthetic FormData via page.evaluate(fetch). This still
// exercises every server-side handler.
//
// Each test that creates an invitation uses a unique recipient email
// (Date.now() suffix) so parallel runs and reruns don't collide on
// the existing-user check or unique-by-recipient assertions.
// ---------------------------------------------------------------------------

test.use({ storageState: STAFF_AUTH });

const LIST_URL = "/dashboard/invitations";
const NEW_URL = "/dashboard/invitations/new";
const NEW_POST_URL = "/dashboard/invitations/new";

type CreateResult = { status: number; hxRedirect: string | null; body: string };

// A YYYY-MM-DD date this many days from now.
function isoDate(daysAhead: number): string {
  return new Date(Date.now() + daysAhead * 86_400_000)
    .toISOString()
    .slice(0, 10);
}

// The start date the list shows, e.g. "Nov 04, 2026".
function listDate(iso: string): string {
  return new Date(`${iso}T12:00:00`).toLocaleDateString("en-US", {
    month: "short",
    day: "2-digit",
    year: "numeric",
  });
}

// Default start date for helper-created invitations.
const DEFAULT_START = isoDate(14);

// New invitations are checked for conflicts with shows and with other live
// invitations. The seeded mock schedule leaves only Tuesday 23:00-24:00 free.
// This file takes 23:00-23:30 and host-onboarding.spec.ts takes 23:30-24:00.
// The tests in this file run one at a time, and each test revokes what it
// creates, so the slot is free for the next test.
const CRUD_SCHEDULE = [
  {
    dayOfWeek: "tuesday",
    weeksOfMonth: [1, 2, 3, 4, 5],
    startTime: "23:00",
    duration: 30,
  },
];

// POST directly to the create endpoint, or to the edit endpoint of one
// invitation. Server returns HX-Redirect on success, or an OOB error banner
// on validation failure.
async function postInvitation(
  page: Page,
  recipientEmail: string,
  schedule: object[],
  startDate: string,
  postUrl: string = NEW_POST_URL,
): Promise<CreateResult> {
  const schedulesJson = JSON.stringify(schedule);
  return await page.evaluate(
    async (args) => {
      const fd = new FormData();
      fd.append("recipient_email", args.recipientEmail);
      fd.append("schedules_json", args.schedulesJson);
      fd.append("schedule_start_date", args.startDate);
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
    { recipientEmail, schedulesJson, startDate, postUrl },
  );
}

// Create an invitation in this file's slot.
async function createInvitation(
  page: Page,
  recipientEmail: string,
  startDate: string = DEFAULT_START,
): Promise<CreateResult> {
  return await postInvitation(page, recipientEmail, CRUD_SCHEDULE, startDate);
}

// Locate the row for a recipient email on the list page.
function rowForRecipient(page: Page, recipientEmail: string) {
  return page.locator("tr", { hasText: recipientEmail });
}

test.describe("Invitations CRUD", () => {
  // The tests share one schedule slot, so they run one at a time.
  test.describe.configure({ mode: "serial" });

  // -------------------------------------------------------------------------
  // Navigation / list
  // -------------------------------------------------------------------------

  test("sidebar link opens the list, and the new form loads", async ({ page }) => {
    await page.goto("/dashboard");
    await page.getByRole("link", { name: /Invitations/i }).click();
    await page.waitForURL(LIST_URL);
    await expect(page.getByText(/INVITATIONS/i).first()).toBeVisible();

    await page.goto(NEW_URL);
    await expect(
      page.getByText(/GENERATE HOST INVITATION/i),
    ).toBeVisible();
    await expect(
      page.locator('input[name="recipient_email"]'),
    ).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Create
  // -------------------------------------------------------------------------

  // The validation of the create form (start date, schedule conflict,
  // malformed or existing email) and the token format are covered by
  // API/Dashboard/Invitations/New/Post/HandlerSpec.hs.
  test("create invitation, list shows a pending row with the start date", async ({ page }) => {
    const email = `e2e-create-${Date.now()}@example.com`;
    const start = isoDate(21);
    await page.goto(NEW_URL);
    const result = await createInvitation(page, email, start);
    expect(result.hxRedirect).toBe(LIST_URL);

    await page.goto(LIST_URL);
    const row = rowForRecipient(page, email);
    await expect(row).toContainText("Pending");
    await expect(row).toContainText(listDate(start));

    // Cleanup: revoke so we don't pile up Pending rows across runs. Revoke
    // removes the row from the page.
    page.on("dialog", (d) => d.accept());
    await row.getByRole("button", { name: /Revoke/i }).click();
    await expect(row).toBeHidden();
  });

  // -------------------------------------------------------------------------
  // Edit page
  // -------------------------------------------------------------------------

  test("edit page saves a new recipient email", async ({ page }) => {
    const origEmail = `e2e-edit-${Date.now()}@example.com`;
    const newEmail = `e2e-edit-${Date.now()}-updated@example.com`;

    await page.goto(NEW_URL);
    await createInvitation(page, origEmail);
    await page.goto(LIST_URL);

    await rowForRecipient(page, origEmail)
      .getByRole("link", { name: /^Edit$/ })
      .click();

    // The form shows the stored email. The schedule editor holds the stored
    // slot and start date, so a save with no schedule change must pass.
    await expect(page.getByText(/EDIT HOST INVITATION/i)).toBeVisible();
    const emailInput = page.locator('input[name="recipient_email"]');
    await expect(emailInput).toHaveValue(origEmail);
    await expect(
      page.locator('input[name="schedule_start_date"]'),
    ).toHaveValue(DEFAULT_START);
    await emailInput.fill(newEmail);
    await page.getByRole("button", { name: /SAVE INVITATION/i }).click();

    // The server redirects to the list with a flash message.
    await expect(page.getByText(/Invitation updated/i)).toBeVisible();
    const row = rowForRecipient(page, newEmail);
    await expect(row).toBeVisible();
    await expect(row).toContainText(listDate(DEFAULT_START));

    // Cleanup
    page.on("dialog", (d) => d.accept());
    await row.getByRole("button", { name: /Revoke/i }).click();
    await expect(row).toBeHidden();
  });

  test("edit changes the schedule and the start date", async ({ page }) => {
    const email = `e2e-edit-schedule-${Date.now()}@example.com`;
    const newStart = isoDate(35);
    await page.goto(NEW_URL);
    await createInvitation(page, email);
    await page.goto(LIST_URL);

    // The new slot is inside the old slot of the same invitation. The
    // conflict check must ignore the invitation that the edit changes.
    const editUrl = await rowForRecipient(page, email)
      .getByRole("link", { name: /^Edit$/ })
      .getAttribute("href");
    expect(editUrl).toMatch(/^\/dashboard\/invitations\/\d+\/edit$/);
    const result = await postInvitation(
      page,
      email,
      [{ ...CRUD_SCHEDULE[0], weeksOfMonth: [1, 3] }],
      newStart,
      editUrl!,
    );
    expect(result.hxRedirect).toBe(LIST_URL);

    await page.goto(LIST_URL);
    const row = rowForRecipient(page, email);
    await expect(row).toContainText("1st & 3rd");
    await expect(row).toContainText(listDate(newStart));

    // Cleanup
    page.on("dialog", (d) => d.accept());
    await row.getByRole("button", { name: /Revoke/i }).click();
    await expect(row).toBeHidden();
  });

  test("edit cancel returns to the list", async ({ page }) => {
    const email = `e2e-edit-cancel-${Date.now()}@example.com`;
    await page.goto(NEW_URL);
    await createInvitation(page, email);
    await page.goto(LIST_URL);

    await rowForRecipient(page, email)
      .getByRole("link", { name: /^Edit$/ })
      .click();
    await expect(
      page.locator('input[name="recipient_email"]'),
    ).toBeVisible();
    await page.getByRole("link", { name: /^CANCEL$/ }).click();

    // The list comes back with the row as it was.
    const row = rowForRecipient(page, email);
    await expect(row.getByRole("link", { name: /^Edit$/ })).toBeVisible();

    // Cleanup
    page.on("dialog", (d) => d.accept());
    await row.getByRole("button", { name: /Revoke/i }).click();
    await expect(row).toBeHidden();
  });

  test("edit rejects existing-user email", async ({ page }) => {
    const email = `e2e-edit-existing-${Date.now()}@example.com`;
    await page.goto(NEW_URL);
    await createInvitation(page, email);
    await page.goto(LIST_URL);

    await rowForRecipient(page, email)
      .getByRole("link", { name: /^Edit$/ })
      .click();
    await page
      .locator('input[name="recipient_email"]')
      .fill("listener@example.com");
    await page.getByRole("button", { name: /SAVE INVITATION/i }).click();

    // Error banner appears. The field hint also names an existing KPBJ
    // account, so the text must be that of the banner.
    await expect(
      page.getByText(/That email belongs to an existing KPBJ account/i),
    ).toBeVisible();

    // Cleanup — full reload, then revoke the row.
    await page.goto(LIST_URL);
    page.on("dialog", (d) => d.accept());
    await rowForRecipient(page, email)
      .getByRole("button", { name: /Revoke/i })
      .click();
    await expect(rowForRecipient(page, email)).toBeHidden();
  });

  // -------------------------------------------------------------------------
  // Resend
  // -------------------------------------------------------------------------

  test("resend shows banner without removing the row", async ({ page }) => {
    const email = `e2e-resend-${Date.now()}@example.com`;
    await page.goto(NEW_URL);
    await createInvitation(page, email);
    await page.goto(LIST_URL);

    const row = rowForRecipient(page, email);
    await row.getByRole("button", { name: /Resend/i }).click();

    await expect(page.getByText(/Re-sent invitation/i)).toBeVisible();
    await expect(row).toBeVisible(); // not removed

    // Cleanup
    page.on("dialog", (d) => d.accept());
    await row.getByRole("button", { name: /Revoke/i }).click();
    await expect(row).toBeHidden();
  });

  // -------------------------------------------------------------------------
  // Regenerate
  // -------------------------------------------------------------------------

  test("regenerate after revoke creates a fresh row", async ({ page }) => {
    const email = `e2e-regen-${Date.now()}@example.com`;
    const start = isoDate(28);
    await page.goto(NEW_URL);
    const created = await createInvitation(page, email, start);
    expect(created.hxRedirect).toBe(LIST_URL);
    await page.goto(LIST_URL);

    // Revoke removes the row from the page. After a reload, the list shows
    // it again as Revoked, with a Regenerate button.
    page.on("dialog", (d) => d.accept());
    await rowForRecipient(page, email)
      .getByRole("button", { name: /Revoke/i })
      .click();
    await expect(rowForRecipient(page, email)).toBeHidden();
    await page.goto(LIST_URL);
    await rowForRecipient(page, email)
      .getByRole("button", { name: /Regenerate/i })
      .click();

    // The server redirects back to the list with a flash message. Wait for it,
    // so the redirect is done before the test reads the list.
    await expect(page.getByText(/Invitation Regenerated/i)).toBeVisible();

    // The new row is pending and keeps the start date.
    const pendingRow = rowForRecipient(page, email).filter({ hasText: "Pending" });
    await expect(pendingRow).toBeVisible();
    await expect(pendingRow).toContainText(listDate(start));

    // Cleanup
    await pendingRow.getByRole("button", { name: /Revoke/i }).click();
    await expect(pendingRow).toBeHidden();
  });
});
