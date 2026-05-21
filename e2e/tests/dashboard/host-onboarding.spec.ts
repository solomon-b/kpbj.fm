import { test, expect, type Browser } from "@playwright/test";
import { STAFF_AUTH, USER_AUTH } from "../../playwright.config";

// ---------------------------------------------------------------------------
// Host onboarding (public invite claim) tests
//
// Covers the unauthenticated /invite/:token flow: invalid token rejection,
// the two-step form, the email-lock behavior, the step-1 validation gate,
// and the full create-account-and-show claim. Plus the already-logged-in
// short-circuit.
//
// Tests start unauthenticated by default. The "create an invitation as
// staff first" prerequisite is satisfied by spinning up a separate browser
// context with STAFF_AUTH inside each test that needs it.
// ---------------------------------------------------------------------------

const NEW_POST_URL = "/dashboard/invitations/new";

// Tuesday 23:00-24:00 is one of the few open slots in the seeded mock
// schedule. checkScheduleConflicts overlaps regardless of weeksOfMonth,
// so we have to pick an actually-empty slot rather than a different
// week-of-month within a busy one.
const DEFAULT_SCHEDULE = [
  {
    dayOfWeek: "tuesday",
    weeksOfMonth: [1, 2, 3, 4, 5],
    startTime: "23:00",
    duration: 60,
  },
];

// Create an invitation in a staff-authenticated side context, then return
// the URL/token for use in the unauthenticated test page. Closes the staff
// context before returning.
async function createInvitationViaStaff(
  browser: Browser,
  recipientEmail: string,
): Promise<{ token: string; inviteUrl: string }> {
  const staffContext = await browser.newContext({
    storageState: STAFF_AUTH,
    permissions: ["clipboard-read", "clipboard-write"],
  });
  const staffPage = await staffContext.newPage();
  try {
    await staffPage.goto("/dashboard/invitations/new");

    const createResult = await staffPage.evaluate(
      async (args) => {
        const fd = new FormData();
        fd.append("recipient_email", args.recipientEmail);
        fd.append("schedules_json", args.schedulesJson);
        const resp = await fetch(args.postUrl, {
          method: "POST",
          body: fd,
        });
        return { hxRedirect: resp.headers.get("HX-Redirect") };
      },
      {
        recipientEmail,
        schedulesJson: JSON.stringify(DEFAULT_SCHEDULE),
        postUrl: NEW_POST_URL,
      },
    );
    expect(createResult.hxRedirect).toBeTruthy();

    await staffPage.goto("/dashboard/invitations");
    await staffPage
      .locator("tr", { hasText: recipientEmail })
      .getByRole("button", { name: /Copy Link|Copied!/ })
      .click();
    const inviteUrl = await staffPage.evaluate(() =>
      navigator.clipboard.readText(),
    );
    const tokenMatch = inviteUrl.match(/\/invite\/(.+)$/);
    return { token: tokenMatch![1], inviteUrl };
  } finally {
    await staffContext.close();
  }
}

test.describe("Host onboarding", () => {
  // -------------------------------------------------------------------------
  // Invalid / missing tokens
  // -------------------------------------------------------------------------

  test("invalid token shows the error page", async ({ page }) => {
    await page.goto("/invite/INV-AAAA-AAAA");
    await expect(
      page.getByText(/invalid|expired|already been used/i),
    ).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Form rendering + locked email
  // -------------------------------------------------------------------------

  test("valid token renders two-step form with schedule preview", async ({
    browser,
    page,
  }) => {
    const recipient = `e2e-onb-render-${Date.now()}@example.com`;
    const { inviteUrl } = await createInvitationViaStaff(browser, recipient);

    await page.goto(inviteUrl);

    // Step 1 visible, step 2 hidden.
    await expect(page.getByText(/Welcome to KPBJ/i)).toBeVisible();
    await expect(page.getByText(/Tell us about your show/i)).toBeHidden();

    // Schedule preview is present (rendered from the invitation's schedule).
    // It's rendered in both step1 and step2; scope to the visible one.
    await expect(
      page.locator("#step1-fields").getByText(/Your Assigned Timeslot/i),
    ).toBeVisible();
  });

  test("email field is pre-filled and read-only", async ({ browser, page }) => {
    const recipient = `e2e-onb-locked-${Date.now()}@example.com`;
    const { inviteUrl } = await createInvitationViaStaff(browser, recipient);

    await page.goto(inviteUrl);

    const emailInput = page.locator('input[name="email"]');
    await expect(emailInput).toHaveValue(recipient);
    await expect(emailInput).toHaveAttribute("readonly", "");
  });

  // -------------------------------------------------------------------------
  // Step-1 validation gate
  // -------------------------------------------------------------------------

  test("Next button does not advance when step 1 is invalid", async ({
    browser,
    page,
  }) => {
    const recipient = `e2e-onb-gate-${Date.now()}@example.com`;
    const { inviteUrl } = await createInvitationViaStaff(browser, recipient);

    await page.goto(inviteUrl);

    // Leave required fields blank, click Next.
    await page.getByRole("button", { name: /Next.*Set Up/i }).click();

    // Step 2 must still be hidden (the reportValidity gate blocked the move).
    await expect(page.getByText(/Tell us about your show/i)).toBeHidden();
  });

  test("Next button does not advance when password is too short", async ({
    browser,
    page,
  }) => {
    const recipient = `e2e-onb-short-${Date.now()}@example.com`;
    const { inviteUrl } = await createInvitationViaStaff(browser, recipient);

    await page.goto(inviteUrl);

    await page.locator('input[name="full_name"]').fill("E2E Host");
    await page.locator('input[name="display_name"]').fill("e2e-host");
    await page.locator('input[name="password"]').fill("short"); // < 8 chars
    await page.locator('input[name="confirm_password"]').fill("short");
    await page.locator('input[name="terms"]').check();

    await page.getByRole("button", { name: /Next.*Set Up/i }).click();
    await expect(page.getByText(/Tell us about your show/i)).toBeHidden();
  });

  test("valid step 1 advances to step 2", async ({ browser, page }) => {
    const recipient = `e2e-onb-advance-${Date.now()}@example.com`;
    const { inviteUrl } = await createInvitationViaStaff(browser, recipient);

    await page.goto(inviteUrl);

    await page.locator('input[name="full_name"]').fill("E2E Host");
    await page.locator('input[name="display_name"]').fill("e2e-host");
    await page.locator('input[name="password"]').fill("longenough1");
    await page.locator('input[name="confirm_password"]').fill("longenough1");
    await page.locator('input[name="terms"]').check();

    await page.getByRole("button", { name: /Next.*Set Up/i }).click();
    await expect(page.getByText(/Tell us about your show/i)).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Mismatched email — server-side enforcement
  // -------------------------------------------------------------------------

  test("server rejects submission when email is changed away from the invitation recipient", async ({
    browser,
    page,
  }) => {
    const recipient = `e2e-onb-mismatch-${Date.now()}@example.com`;
    const { token } = await createInvitationViaStaff(browser, recipient);

    // Skip the form and POST directly with a different email.
    await page.goto("/"); // need a page context for fetch
    const result = await page.evaluate(
      async (args) => {
        const fd = new FormData();
        fd.append("email", args.tamperedEmail);
        fd.append("full_name", "E2E Tamper");
        fd.append("display_name", "e2e-tamper");
        fd.append("password", "longenough1");
        fd.append("confirm_password", "longenough1");
        fd.append("terms", "on");
        fd.append("show_title", "E2E Tamper Show " + Date.now());
        fd.append("show_description", "test");
        fd.append("show_tags", "test");
        const resp = await fetch(args.postUrl, {
          method: "POST",
          body: fd,
        });
        return {
          hxRedirect: resp.headers.get("HX-Redirect"),
          body: await resp.text(),
        };
      },
      {
        tamperedEmail: `tamper-${Date.now()}@example.com`,
        postUrl: `/invite/${token}`,
      },
    );

    // No redirect; banner explains the mismatch.
    expect(result.hxRedirect).toBeNull();
    expect(result.body).toMatch(/bound to a different email/i);
  });

  // -------------------------------------------------------------------------
  // Full happy path
  // -------------------------------------------------------------------------

  // NOTE: this test leaves a show in the DB (Tuesday 23:00). Subsequent
  // runs will fail on a schedule conflict until the show is cleaned up.
  // CI re-seeds mock data each run; locally, run `just dev-mock-data`
  // to reset if this test starts failing with a conflict error.
  test("submit creates account, claims invitation, lands on show dashboard", async ({
    browser,
    page,
  }) => {
    const stamp = Date.now();
    const recipient = `e2e-onb-claim-${stamp}@example.com`;
    const showTitle = `E2E Claim Show ${stamp}`;
    const { token, inviteUrl } = await createInvitationViaStaff(
      browser,
      recipient,
    );

    await page.goto(inviteUrl);

    // POST directly — driving the multipart submit through the browser
    // requires juggling the Alpine step state and a file upload. The
    // POST exercises all server-side handlers and the redirect path.
    const result = await page.evaluate(
      async (args) => {
        const fd = new FormData();
        fd.append("email", args.recipient);
        fd.append("full_name", "E2E Claim Host");
        fd.append("display_name", `e2e-claim-${args.stamp}`);
        fd.append("password", "longenough1");
        fd.append("confirm_password", "longenough1");
        fd.append("terms", "on");
        fd.append("show_title", args.showTitle);
        fd.append("show_description", "Claimed by e2e");
        fd.append("show_tags", "e2e, test");
        const resp = await fetch(args.postUrl, {
          method: "POST",
          body: fd,
        });
        return {
          hxRedirect: resp.headers.get("HX-Redirect"),
          body: await resp.text(),
        };
      },
      {
        recipient,
        stamp,
        showTitle,
        postUrl: `/invite/${token}`,
      },
    );

    // Success: server returns HX-Redirect to the new show's dashboard.
    if (!result.hxRedirect) {
      throw new Error(
        `Onboarding POST did not redirect. Body:\n${result.body.slice(0, 1500)}`,
      );
    }
    expect(result.hxRedirect).toMatch(/\/dashboard\/shows\/\d+\//);

    // Hitting the same invite a second time should fail (single-use).
    // Send a complete form so we get past multipart parsing and hit
    // lookupInvitation, which is the actual single-use enforcer.
    const secondAttempt = await page.evaluate(
      async (args) => {
        const fd = new FormData();
        fd.append("email", args.recipient);
        fd.append("full_name", "Second Try");
        fd.append("display_name", "second-try");
        fd.append("password", "longenough1");
        fd.append("show_title", "Second Try Show");
        const resp = await fetch(args.postUrl, { method: "POST", body: fd });
        return await resp.text();
      },
      { recipient, postUrl: `/invite/${token}` },
    );
    expect(secondAttempt).toMatch(/invalid|expired|already been used/i);
  });

  // -------------------------------------------------------------------------
  // Already-logged-in short-circuit
  // -------------------------------------------------------------------------

  test("already-logged-in user sees the 'you already have an account' screen", async ({
    browser,
  }) => {
    const recipient = `e2e-onb-loggedin-${Date.now()}@example.com`;
    const { inviteUrl } = await createInvitationViaStaff(browser, recipient);

    // Visit the invite link with a USER session.
    const userContext = await browser.newContext({ storageState: USER_AUTH });
    const userPage = await userContext.newPage();
    try {
      await userPage.goto(inviteUrl);
      await expect(
        userPage.getByText(/You already have an account/i),
      ).toBeVisible();
    } finally {
      await userContext.close();
    }
  });
});
