-- Store Products
-- A small catalog of merch items for the webstore.
-- Mix of simple products (no variants) and products with size/color variants.

-- Simple products (no variants, inventory on the product itself)
INSERT INTO products (name, slug, description, base_price_cents, weight_oz, category, inventory_count, is_active, sort_order)
VALUES
  (
    'KPBJ 95.9 Bumper Sticker',
    'kpbj-bumper-sticker',
    'Show your KPBJ pride on your car, laptop, or water bottle. Weatherproof vinyl, 8" x 3".',
    300,
    1,
    'accessories',
    50,
    TRUE,
    1
  ),
  (
    'KPBJ Enamel Pin',
    'kpbj-enamel-pin',
    'Hard enamel pin with the KPBJ dial logo. 1.25" with butterfly clutch backing.',
    800,
    1,
    'accessories',
    25,
    TRUE,
    2
  ),
  (
    'KPBJ Tote Bag',
    'kpbj-tote-bag',
    'Heavy-duty canvas tote with screen-printed KPBJ logo. Perfect for records, groceries, or whatever you carry.',
    2200,
    8,
    'accessories',
    15,
    TRUE,
    3
  ),
  (
    'KPBJ Coffee Mug',
    'kpbj-coffee-mug',
    'Ceramic 12oz mug. Dishwasher and microwave safe.',
    1500,
    12,
    'accessories',
    0,
    TRUE,
    4
  ),
  -- Inactive product (should not appear in store listing)
  (
    'KPBJ Vintage Poster (Discontinued)',
    'kpbj-vintage-poster',
    'Limited edition poster from 2024 launch. No longer available.',
    4500,
    4,
    'collectibles',
    3,
    FALSE,
    99
  );

-- Product with variants: T-Shirt (sizes S/M/L/XL)
INSERT INTO products (name, slug, description, base_price_cents, weight_oz, category, inventory_count, is_active, sort_order)
VALUES
  (
    'KPBJ Logo T-Shirt',
    'kpbj-logo-tshirt',
    'Black tee with white KPBJ 95.9 logo. 100% cotton, pre-shrunk.',
    2500,
    6,
    'apparel',
    0,
    TRUE,
    0
  );

-- Option type: Size
INSERT INTO product_option_types (product_id, name, sort_order)
VALUES
  ((SELECT id FROM products WHERE slug = 'kpbj-logo-tshirt'), 'Size', 1);

-- Option values: S, M, L, XL
INSERT INTO product_option_values (option_type_id, value, sort_order)
SELECT ot.id, v.value, v.sort_order
FROM product_option_types ot,
  (VALUES ('S', 1), ('M', 2), ('L', 3), ('XL', 4)) AS v(value, sort_order)
WHERE ot.product_id = (SELECT id FROM products WHERE slug = 'kpbj-logo-tshirt')
  AND ot.name = 'Size';

-- Variants for each size (deleted_at = NULL means active)
INSERT INTO product_variants (product_id, label, price_cents, inventory_count, sku, weight_oz, sort_order)
VALUES
  ((SELECT id FROM products WHERE slug = 'kpbj-logo-tshirt'), 'S',  NULL, 8,  'TSHIRT-S',  6, 1),
  ((SELECT id FROM products WHERE slug = 'kpbj-logo-tshirt'), 'M',  NULL, 12, 'TSHIRT-M',  6, 2),
  ((SELECT id FROM products WHERE slug = 'kpbj-logo-tshirt'), 'L',  NULL, 5,  'TSHIRT-L',  7, 3),
  ((SELECT id FROM products WHERE slug = 'kpbj-logo-tshirt'), 'XL', NULL, 0,  'TSHIRT-XL', 8, 4);

-- Link variants to option values
INSERT INTO product_variant_options (variant_id, option_value_id)
SELECT pv.id, pov.id
FROM product_variants pv
JOIN product_option_values pov
  ON pov.value = pv.label
JOIN product_option_types pot
  ON pot.id = pov.option_type_id
WHERE pv.product_id = (SELECT id FROM products WHERE slug = 'kpbj-logo-tshirt')
  AND pot.product_id = (SELECT id FROM products WHERE slug = 'kpbj-logo-tshirt');

-- Product images (hero images for listing cards)
INSERT INTO product_images (product_id, image_path, alt_text, sort_order)
VALUES
  ((SELECT id FROM products WHERE slug = 'kpbj-bumper-sticker'), 'images/product-images/2026/03/01/kpbj-bumper-sticker_mock.jpg', 'KPBJ bumper sticker', 1),
  ((SELECT id FROM products WHERE slug = 'kpbj-enamel-pin'),     'images/product-images/2026/03/01/kpbj-enamel-pin_mock.jpg',     'KPBJ enamel pin',     1),
  ((SELECT id FROM products WHERE slug = 'kpbj-tote-bag'),       'images/product-images/2026/03/01/kpbj-tote-bag_mock.jpg',       'KPBJ tote bag',       1),
  ((SELECT id FROM products WHERE slug = 'kpbj-logo-tshirt'),    'images/product-images/2026/03/01/kpbj-logo-tshirt_mock.jpg',    'KPBJ logo t-shirt',   1),
  -- Second image for t-shirt (back view)
  ((SELECT id FROM products WHERE slug = 'kpbj-logo-tshirt'),    'images/product-images/2026/03/01/kpbj-logo-tshirt_back.jpg',    'KPBJ logo t-shirt back', 2);
