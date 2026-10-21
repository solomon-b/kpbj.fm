import { test, expect, type Page } from "@playwright/test";
import { STAFF_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Invitations CRUD tests
//
// Covers list, create, inline edit, resend, revoke, and regenerate for
// host invitations. Uses STAFF_AUTH (only staff+ can manage invitations).
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

// POST directly to the create endpoint. Server returns HX-Redirect on
// success, or an OOB error banner on validation failure.
async function postInvitation(
  page: Page,
  recipientEmail: string,
  schedule: object[],
  startDate: string,
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
    { recipientEmail, schedulesJson, startDate, postUrl: NEW_POST_URL },
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

// Click the row's Copy Link button and read the URL from the clipboard.
// The Alpine @click directive doesn't survive as a readable attribute
// after init, so we exercise the actual copy mechanism instead.
async function inviteUrlForRow(
  page: Page,
  recipientEmail: string,
): Promise<string> {
  await page
    .context()
    .grantPermissions(["clipboard-read", "clipboard-write"]);
  await rowForRecipient(page, recipientEmail)
    .getByRole("button", { name: /Copy Link|Copied!/ })
    .click();
  return await page.evaluate(() => navigator.clipboard.readText());
}

test.describe("Invitations CRUD", () => {
  // The tests share one schedule slot, so they run one at a time.
  test.describe.configure({ mode: "serial" });

  // -------------------------------------------------------------------------
  // Navigation / list
  // -------------------------------------------------------------------------

  test("sidebar has Invitations link", async ({ page }) => {
    await page.goto("/dashboard");
    await expect(
      page.getByRole("link", { name: /Invitations/i }),
    ).toBeVisible();
  });

  test("list page loads", async ({ page }) => {
    await page.goto(LIST_URL);
    await expect(page.getByText(/INVITATIONS/i).first()).toBeVisible();
  });

  test("new invitation form page loads", async ({ page }) => {
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

  test("create invitation, list shows pending row", async ({ page }) => {
    const email = `e2e-create-${Date.now()}@example.com`;
    await page.goto(NEW_URL);
    const result = await createInvitation(page, email);
    expect(result.hxRedirect).toBe(LIST_URL);

    await page.goto(LIST_URL);
    const row = rowForRecipient(page, email);
    await expect(row).toBeVisible();
    await expect(row).toContainText("Pending");

    // Cleanup: revoke so we don't pile up Pending rows across runs.
    page.on("dialog", (d) => d.accept());
    await row.getByRole("button", { name: /Revoke/i }).click();
    await expect(row).toBeHidden();
  });

  test("list shows the start date", async ({ page }) => {
    const email = `e2e-start-${Date.now()}@example.com`;
    const start = isoDate(21);
    await page.goto(NEW_URL);
    const result = await createInvitation(page, email, start);
    expect(result.hxRedirect).toBe(LIST_URL);

    await page.goto(LIST_URL);
    const row = rowForRecipient(page, email);
    await expect(row).toContainText(listDate(start));

    // Cleanup
    page.on("dialog", (d) => d.accept());
    await row.getByRole("button", { name: /Revoke/i }).click();
    await expect(row).toBeHidden();
  });

  test("create requires a start date", async ({ page }) => {
    await page.goto(NEW_URL);
    const result = await postInvitation(
      page,
      `e2e-no-start-${Date.now()}@example.com`,
      CRUD_SCHEDULE,
      "",
    );
    expect(result.hxRedirect).toBeNull();
    expect(result.body).toMatch(/Schedule start date is required/);
  });

  test("create rejects a slot that a pending invitation holds", async ({ page }) => {
    const firstEmail = `e2e-conflict-a-${Date.now()}@example.com`;
    await page.goto(NEW_URL);
    const first = await createInvitation(page, firstEmail);
    expect(first.hxRedirect).toBe(LIST_URL);

    const second = await postInvitation(
      page,
      `e2e-conflict-b-${Date.now()}@example.com`,
      CRUD_SCHEDULE,
      DEFAULT_START,
    );
    expect(second.hxRedirect).toBeNull();
    expect(second.body).toMatch(/Schedule conflict:.*pending invitation/);

    // Cleanup
    await page.goto(LIST_URL);
    page.on("dialog", (d) => d.accept());
    await rowForRecipient(page, firstEmail)
      .getByRole("button", { name: /Revoke/i })
      .click();
    await expect(rowForRecipient(page, firstEmail)).toBeHidden();
  });

  test("create blocks existing-user email", async ({ page }) => {
    // listener@example.com is a known mock user.
    await page.goto(NEW_URL);
    const result = await createInvitation(page, "listener@example.com");
    // Validation error renders an OOB banner, no HX-Redirect.
    expect(result.hxRedirect).toBeNull();
    expect(result.body).toMatch(/existing KPBJ account/i);
  });

  test("create rejects malformed email", async ({ page }) => {
    await page.goto(NEW_URL);
    const result = await createInvitation(page, "not-an-email");
    expect(result.hxRedirect).toBeNull();
    expect(result.body).toMatch(/not a valid email/i);
  });

  test("invite URL uses INV-XXXX-XXXX token format", async ({ page }) => {
    const email = `e2e-token-${Date.now()}@example.com`;
    await page.goto(NEW_URL);
    await createInvitation(page, email);
    await page.goto(LIST_URL);

    const url = await inviteUrlForRow(page, email);
    expect(url).toMatch(
      /\/invite\/INV-[23456789ABCDEFGHJKMNPQRSTVWXYZ]{4}-[23456789ABCDEFGHJKMNPQRSTVWXYZ]{4}$/,
    );

    // Cleanup
    page.on("dialog", (d) => d.accept());
    await rowForRecipient(page, email)
      .getByRole("button", { name: /Revoke/i })
      .click();
    await expect(rowForRecipient(page, email)).toBeHidden();
  });

  // -------------------------------------------------------------------------
  // Inline edit
  // -------------------------------------------------------------------------

  test("inline edit updates recipient email", async ({ page }) => {
    const origEmail = `e2e-edit-${Date.now()}@example.com`;
    const newEmail = `e2e-edit-${Date.now()}-updated@example.com`;

    await page.goto(NEW_URL);
    await createInvitation(page, origEmail);
    await page.goto(LIST_URL);

    const row = rowForRecipient(page, origEmail);
    await row.getByRole("button", { name: /^Edit$/ }).click();

    // After the row swap the input is in place of the row cells.
    const emailInput = page.locator('input[name="recipient_email"]');
    await expect(emailInput).toBeVisible();
    await emailInput.fill(newEmail);
    await page.getByRole("button", { name: /^SAVE$/ }).click();

    // Server returns the new row + OOB banner.
    await expect(page.getByText(/Recipient email saved/i)).toBeVisible();
    await expect(rowForRecipient(page, newEmail)).toBeVisible();

    // Cleanup
    page.on("dialog", (d) => d.accept());
    await rowForRecipient(page, newEmail)
      .getByRole("button", { name: /Revoke/i })
      .click();
    await expect(rowForRecipient(page, newEmail)).toBeHidden();
  });

  test("inline edit cancel restores the original row", async ({ page }) => {
    const email = `e2e-edit-cancel-${Date.now()}@example.com`;
    await page.goto(NEW_URL);
    await createInvitation(page, email);
    await page.goto(LIST_URL);

    const row = rowForRecipient(page, email);
    await row.getByRole("button", { name: /^Edit$/ }).click();
    await expect(
      page.locator('input[name="recipient_email"]'),
    ).toBeVisible();
    await page.getByRole("button", { name: /^CANCEL$/ }).click();

    // The normal row fragment comes back; Edit button visible again.
    await expect(
      rowForRecipient(page, email).getByRole("button", { name: /^Edit$/ }),
    ).toBeVisible();

    // Cleanup
    page.on("dialog", (d) => d.accept());
    await rowForRecipient(page, email)
      .getByRole("button", { name: /Revoke/i })
      .click();
    await expect(rowForRecipient(page, email)).toBeHidden();
  });

  test("inline edit rejects existing-user email", async ({ page }) => {
    const email = `e2e-edit-existing-${Date.now()}@example.com`;
    await page.goto(NEW_URL);
    await createInvitation(page, email);
    await page.goto(LIST_URL);

    const row = rowForRecipient(page, email);
    await row.getByRole("button", { name: /^Edit$/ }).click();
    await page
      .locator('input[name="recipient_email"]')
      .fill("listener@example.com");
    await page.getByRole("button", { name: /^SAVE$/ }).click();

    // Error banner appears.
    await expect(page.getByText(/existing KPBJ account/i)).toBeVisible();

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
  // Revoke + regenerate
  // -------------------------------------------------------------------------

  test("revoke removes row from list", async ({ page }) => {
    const email = `e2e-revoke-${Date.now()}@example.com`;
    await page.goto(NEW_URL);
    await createInvitation(page, email);
    await page.goto(LIST_URL);

    const row = rowForRecipient(page, email);
    await expect(row).toBeVisible();
    page.on("dialog", (d) => d.accept());
    await row.getByRole("button", { name: /Revoke/i }).click();
    await expect(row).toBeHidden();
  });

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
