import { test, expect } from "@playwright/test";
import { CROSS_BROWSER } from "../playwright.config";

// ---------------------------------------------------------------------------
// Store pages (/store, /store/products/:slug, /store/cart)
// ---------------------------------------------------------------------------
// Mock data has 6 products: 4 simple active (1 out of stock), 1 inactive,
// 1 with size variants (S/M/L/XL where XL is out of stock).
// The store listing shows only active products.
//
// The 404 for a missing or inactive product is covered by
// API/Store/Products/Slug/Get/HandlerSpec.hs.

test.describe("Store listing", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/store");
  });

  test("displays the active products", async ({ page }) => {
    await expect(page.locator("h1", { hasText: "STORE" })).toBeAttached();
    await expect(page.getByText("Support KPBJ")).toBeVisible();

    // 5 active products in mock data (bumper sticker, enamel pin, tote bag,
    // coffee mug, logo t-shirt). The inactive poster does not appear.
    const cards = page.locator("#store-content-container a.block");
    await expect(cards).toHaveCount(5);
    await expect(page.getByText("Vintage Poster")).not.toBeVisible();
    for (const name of [
      "KPBJ 95.9 Bumper Sticker",
      "KPBJ Enamel Pin",
      "KPBJ Tote Bag",
      "KPBJ Logo T-Shirt",
    ]) {
      await expect(page.getByText(name)).toBeVisible();
    }

    // Bumper sticker is $3.00 and enamel pin is $8.00.
    await expect(page.getByText("$3.00")).toBeVisible();
    await expect(page.getByText("$8.00")).toBeVisible();

    // Coffee mug has 0 inventory.
    const mugCard = page.locator("a.block", { hasText: "KPBJ Coffee Mug" });
    await expect(mugCard.getByText("OUT OF STOCK")).toBeVisible();
  });

  test("clicking a product card navigates to the detail page", async ({ page }) => {
    await page.getByText("KPBJ Enamel Pin").click();
    await page.waitForURL(/\/store\/products\/kpbj-enamel-pin/);
  });
});

// ---------------------------------------------------------------------------
// Product detail page
// ---------------------------------------------------------------------------

test.describe("Product detail (simple)", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/store/products/kpbj-enamel-pin");
  });

  test("displays the product", async ({ page }) => {
    await expect(page.locator("h1", { hasText: "KPBJ Enamel Pin" })).toBeVisible();
    await expect(page.getByText("$8.00")).toBeVisible();
    await expect(page.getByText(/Hard enamel pin/)).toBeVisible();
    await expect(page.getByText("25 in stock")).toBeVisible();
    await expect(page.getByText("accessories")).toBeVisible();
    await expect(page.locator('img[alt="KPBJ enamel pin"]')).toBeVisible();

    // The quantity display has x-text="quantity".
    await expect(page.locator("[x-text='quantity']")).toHaveText("1");
    await expect(page.getByText("ADD TO CART")).toBeVisible();
  });

  test("breadcrumb navigates back to store listing", async ({ page }) => {
    // Target the breadcrumb nav inside #main-content to avoid matching
    // the mobile menu and desktop nav "Store" links.
    await page.locator("#main-content nav a", { hasText: "Store" }).click();
    await page.waitForURL(/\/store$/);
  });
});

test.describe("Product detail (with variants)", () => {
  test("displays the variant selector and thumbnails", async ({ page }) => {
    await page.goto("/store/products/kpbj-logo-tshirt");

    await expect(page.locator("h1", { hasText: "KPBJ Logo T-Shirt" })).toBeVisible();

    // "Select..." + S, M, L, XL = 5 options.
    await expect(page.getByText("Size")).toBeVisible();
    await expect(page.locator("select").first().locator("option")).toHaveCount(5);
    await expect(page.getByText("Select options to see availability")).toBeVisible();

    // NOTE: Variant selection reactivity (selecting M shows "12 in stock",
    // selecting XL shows "OUT OF STOCK") is not tested here because Playwright's
    // selectOption doesn't trigger Alpine's x-model on x-for-rendered selects.
    // The variant resolver logic is covered by server-side property tests in
    // ProductsSpec.hs.

    // The t-shirt has 2 images, so the thumbnail strip renders.
    await expect(page.locator("button img")).toHaveCount(2);
  });
});

// ---------------------------------------------------------------------------
// Add to cart + cart page
// ---------------------------------------------------------------------------
// Each test gets a new browser context, so the cart in localStorage starts
// empty.

test.describe("Cart functionality", () => {
  test("adding a product saves it to localStorage with its quantity", { tag: CROSS_BROWSER }, async ({ page }) => {
    await page.goto("/store/products/kpbj-bumper-sticker");

    // Increment the quantity to 3.
    const plusButton = page.locator("button", { hasText: "+" });
    await plusButton.click();
    await plusButton.click();

    await page.getByText("ADD TO CART").click();
    await expect(page.getByText("ADDED")).toBeVisible();

    const cart = await page.evaluate(() => localStorage.getItem("kpbj-cart"));
    const items = JSON.parse(cart!);
    expect(items).toHaveLength(1);
    expect(items[0].quantity).toBe(3);
  });

  test("an added product appears in the cart with checkout", { tag: CROSS_BROWSER }, async ({ page }) => {
    await page.goto("/store/products/kpbj-enamel-pin");
    await page.getByText("ADD TO CART").click();
    await expect(page.getByText("ADDED")).toBeVisible();

    await page.goto("/store/cart");
    await expect(page.getByText("Loading cart...")).not.toBeVisible({ timeout: 10_000 });

    // The unit price, line total, and subtotal all show $8.00 for one item.
    await expect(page.getByText("KPBJ Enamel Pin")).toBeVisible();
    await expect(page.getByText("$8.00").first()).toBeVisible();
    await expect(page.getByText("Subtotal")).toBeVisible();
    await expect(page.getByRole("link", { name: "CHECKOUT" })).toBeVisible();
  });

  test("an empty cart links back to the store", async ({ page }) => {
    await page.goto("/store/cart");
    await expect(page.getByText("Your cart is empty.")).toBeVisible();

    await page.getByText("Continue Shopping").click();
    await page.waitForURL(/\/store$/);
  });
});
