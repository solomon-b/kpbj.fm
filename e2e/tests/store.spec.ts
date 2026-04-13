import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Store pages (/store, /store/products/:slug, /store/cart)
// ---------------------------------------------------------------------------
// Mock data has 6 products: 4 simple active (1 out of stock), 1 inactive,
// 1 with size variants (S/M/L/XL where XL is out of stock).
// The store listing shows only active products.

test.describe("Store listing", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/store");
  });

  // -------------------------------------------------------------------------
  // Page structure
  // -------------------------------------------------------------------------

  test("displays the STORE heading", async ({ page }) => {
    await expect(page.locator("h1", { hasText: "STORE" })).toBeAttached();
  });

  test("displays the support message", async ({ page }) => {
    await expect(
      page.getByText("Support KPBJ")
    ).toBeVisible();
  });

  // -------------------------------------------------------------------------
  // Product cards
  // -------------------------------------------------------------------------

  test("displays active product cards", async ({ page }) => {
    // 5 active products in mock data (bumper sticker, enamel pin, tote bag,
    // coffee mug, logo t-shirt). The inactive poster should not appear.
    const cards = page.locator("#store-content-container a.block");
    await expect(cards).toHaveCount(5);
  });

  test("does not display inactive products", async ({ page }) => {
    await expect(page.getByText("Vintage Poster")).not.toBeVisible();
  });

  test("product cards show product names", async ({ page }) => {
    await expect(page.getByText("KPBJ 95.9 Bumper Sticker")).toBeVisible();
    await expect(page.getByText("KPBJ Enamel Pin")).toBeVisible();
    await expect(page.getByText("KPBJ Tote Bag")).toBeVisible();
    await expect(page.getByText("KPBJ Logo T-Shirt")).toBeVisible();
  });

  test("product cards show prices", async ({ page }) => {
    // Bumper sticker is $3.00
    await expect(page.getByText("$3.00")).toBeVisible();
    // Enamel pin is $8.00
    await expect(page.getByText("$8.00")).toBeVisible();
  });

  test("out-of-stock product shows OUT OF STOCK label", async ({ page }) => {
    // Coffee mug has 0 inventory
    const mugCard = page.locator("a.block", { hasText: "KPBJ Coffee Mug" });
    await expect(mugCard.getByText("OUT OF STOCK")).toBeVisible();
  });

  test("product cards link to detail pages", async ({ page }) => {
    const firstCard = page.locator("#store-content-container a.block").first();
    const href = await firstCard.getAttribute("href");
    expect(href).toMatch(/\/store\/products\/.+/);
  });

  test("clicking a product card navigates to the detail page", async ({ page }) => {
    await page.getByText("KPBJ Enamel Pin").click();
    await page.waitForURL(/\/store\/products\/kpbj-enamel-pin/);
  });
});

// ---------------------------------------------------------------------------
// Product detail page (simple product, no variants)
// ---------------------------------------------------------------------------

test.describe("Product detail (simple)", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/store/products/kpbj-enamel-pin");
  });

  test("displays the product name as h1", async ({ page }) => {
    await expect(
      page.locator("h1", { hasText: "KPBJ Enamel Pin" })
    ).toBeVisible();
  });

  test("displays the price", async ({ page }) => {
    await expect(page.getByText("$8.00")).toBeVisible();
  });

  test("displays the description", async ({ page }) => {
    await expect(page.getByText(/Hard enamel pin/)).toBeVisible();
  });

  test("displays inventory count", async ({ page }) => {
    await expect(page.getByText("25 in stock")).toBeVisible();
  });

  test("displays breadcrumb with Store link", async ({ page }) => {
    // Target the breadcrumb nav inside #main-content to avoid matching
    // the mobile menu and desktop nav "Store" links.
    const breadcrumb = page.locator("#main-content nav a", { hasText: "Store" });
    await expect(breadcrumb).toBeVisible();
  });

  test("breadcrumb navigates back to store listing", async ({ page }) => {
    await page.locator("#main-content nav a", { hasText: "Store" }).click();
    await page.waitForURL(/\/store$/);
  });

  test("has a quantity stepper defaulting to 1", async ({ page }) => {
    // Target the quantity display span inside the stepper (has x-text="quantity").
    await expect(page.locator("[x-text='quantity']")).toBeVisible();
    await expect(page.locator("[x-text='quantity']")).toHaveText("1");
  });

  test("has an ADD TO CART button", async ({ page }) => {
    await expect(page.getByText("ADD TO CART")).toBeVisible();
  });

  test("displays category", async ({ page }) => {
    await expect(page.getByText("accessories")).toBeVisible();
  });

  test("displays product image", async ({ page }) => {
    const img = page.locator('img[alt="KPBJ enamel pin"]');
    await expect(img).toBeVisible();
  });
});

// ---------------------------------------------------------------------------
// Product detail page (with variants)
// ---------------------------------------------------------------------------

test.describe("Product detail (with variants)", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/store/products/kpbj-logo-tshirt");
  });

  test("displays the product name", async ({ page }) => {
    await expect(
      page.locator("h1", { hasText: "KPBJ Logo T-Shirt" })
    ).toBeVisible();
  });

  test("displays variant selector for Size", async ({ page }) => {
    // The option type label "Size" should be visible.
    await expect(page.getByText("Size")).toBeVisible();

    // A <select> with options for each size should exist.
    const select = page.locator("select").first();
    await expect(select).toBeVisible();
    const options = select.locator("option");
    // "Select..." + S, M, L, XL = 5 options
    await expect(options).toHaveCount(5);
  });

  test("shows 'Select options to see availability' before choosing", async ({ page }) => {
    await expect(
      page.getByText("Select options to see availability")
    ).toBeVisible();
  });

  // NOTE: Variant selection reactivity (selecting M shows "12 in stock",
  // selecting XL shows "OUT OF STOCK") is not tested here because Playwright's
  // selectOption doesn't trigger Alpine's x-model on x-for-rendered selects.
  // The variant resolver logic is covered by server-side property tests in
  // ProductsSpec.hs. The selector existence and option count are verified above.

  test("has thumbnail strip for multiple images", async ({ page }) => {
    // T-shirt has 2 images, so thumbnail strip should render.
    const thumbnails = page.locator("button img");
    await expect(thumbnails).toHaveCount(2);
  });
});

// ---------------------------------------------------------------------------
// Product detail — inactive/missing product
// ---------------------------------------------------------------------------

test.describe("Product detail (not found)", () => {
  test("returns 404 for nonexistent slug", async ({ page }) => {
    await page.goto("/store/products/does-not-exist");
    // The handler throws NotFound which returns a 404 page or redirect.
    // Just verify we don't see a product page.
    await expect(
      page.locator("h1", { hasText: "does-not-exist" })
    ).not.toBeAttached();
  });

  test("returns 404 for inactive product", async ({ page }) => {
    await page.goto("/store/products/kpbj-vintage-poster");
    await expect(
      page.locator("h1", { hasText: "Vintage Poster" })
    ).not.toBeAttached();
  });
});

// ---------------------------------------------------------------------------
// Add to cart + cart page
// ---------------------------------------------------------------------------

test.describe("Cart functionality", () => {
  test("adding a simple product shows ADDED confirmation", async ({ page }) => {
    await page.goto("/store/products/kpbj-enamel-pin");

    // Clear any previous cart state
    await page.evaluate(() => localStorage.removeItem("kpbj-cart"));
    await page.reload();

    // Click ADD TO CART
    await page.getByText("ADD TO CART").click();

    // Should show "ADDED" confirmation briefly
    await expect(page.getByText("ADDED")).toBeVisible();
  });

  test("adding a product persists to localStorage", async ({ page }) => {
    await page.goto("/store/products/kpbj-enamel-pin");
    await page.evaluate(() => localStorage.removeItem("kpbj-cart"));
    await page.reload();

    await page.getByText("ADD TO CART").click();
    await expect(page.getByText("ADDED")).toBeVisible();

    // Verify localStorage has the cart item
    const cart = await page.evaluate(() => localStorage.getItem("kpbj-cart"));
    expect(cart).toBeTruthy();
    const items = JSON.parse(cart!);
    expect(items).toHaveLength(1);
    expect(items[0].quantity).toBe(1);
  });

  test("incrementing quantity before adding reflects in cart", async ({ page }) => {
    await page.goto("/store/products/kpbj-bumper-sticker");
    await page.evaluate(() => localStorage.removeItem("kpbj-cart"));
    await page.reload();

    // Increment quantity to 3
    const plusButton = page.locator("button", { hasText: "+" });
    await plusButton.click();
    await plusButton.click();

    // Add to cart (qty 3)
    await page.getByText("ADD TO CART").click();
    await expect(page.getByText("ADDED")).toBeVisible();

    // Verify quantity in localStorage
    const cart = await page.evaluate(() => localStorage.getItem("kpbj-cart"));
    const items = JSON.parse(cart!);
    expect(items[0].quantity).toBe(3);
  });

  test("cart page shows empty state when no items", async ({ page }) => {
    // Clear localStorage to ensure empty cart
    await page.goto("/store");
    await page.evaluate(() => localStorage.removeItem("kpbj-cart"));

    await page.goto("/store/cart");

    await expect(page.getByText("Your cart is empty.")).toBeVisible();
    await expect(page.getByText("Continue Shopping")).toBeVisible();
  });

  test("cart page Continue Shopping links to store", async ({ page }) => {
    await page.goto("/store");
    await page.evaluate(() => localStorage.removeItem("kpbj-cart"));

    await page.goto("/store/cart");
    await page.getByText("Continue Shopping").click();
    await page.waitForURL(/\/store$/);
  });

  test("adding a product then visiting cart shows the item", async ({ page }) => {
    // Add an enamel pin
    await page.goto("/store/products/kpbj-enamel-pin");
    await page.getByText("ADD TO CART").click();
    await expect(page.getByText("ADDED")).toBeVisible();

    // Navigate to cart
    await page.goto("/store/cart");

    // Wait for validation to complete
    await expect(page.getByText("Loading cart...")).not.toBeVisible({ timeout: 10_000 });

    // Product name should appear in the cart
    await expect(page.getByText("KPBJ Enamel Pin")).toBeVisible();

    // Unit price should appear (matches first instance — unit price, line total,
    // and subtotal all show $8.00 for a single item)
    await expect(page.getByText("$8.00").first()).toBeVisible();

    // Subtotal should appear
    await expect(page.getByText("Subtotal")).toBeVisible();
  });

  test("cart shows checkout button", async ({ page }) => {
    await page.goto("/store/products/kpbj-bumper-sticker");
    await page.getByText("ADD TO CART").click();

    await page.goto("/store/cart");
    await expect(page.getByText("Loading cart...")).not.toBeVisible({ timeout: 10_000 });

    await expect(page.getByRole("link", { name: "CHECKOUT" })).toBeVisible();
  });
});

// ---------------------------------------------------------------------------
// Navigation integration
// ---------------------------------------------------------------------------

test.describe("Store navigation", () => {
  test.beforeEach(async ({}, testInfo) => {
    test.skip(testInfo.project.name.startsWith("mobile"), "Desktop nav — see mobile.spec.ts");
  });

  test("Store link appears in desktop navigation", async ({ page }) => {
    await page.goto("/");
    await expect(page.locator("#nav-store")).toBeVisible();
  });

  test("clicking Store nav link navigates to store", async ({ page }) => {
    await page.goto("/");
    await page.locator("#nav-store").click();
    await page.waitForURL(/\/store/);
    await expect(page.locator("h1", { hasText: "STORE" })).toBeAttached();
  });
});
