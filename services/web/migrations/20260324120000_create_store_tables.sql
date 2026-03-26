-- Webstore Phase 1: All store tables.
--
-- Products with variants, option types/values, images, orders, order items,
-- and a single-row store settings table.

--------------------------------------------------------------------------------
-- Domains

CREATE DOMAIN percentage AS NUMERIC CHECK (VALUE >= 0 AND VALUE <= 1);

--------------------------------------------------------------------------------
-- Products

CREATE TABLE products (
    id SERIAL8 PRIMARY KEY,
    name TEXT NOT NULL,
    slug TEXT NOT NULL UNIQUE,
    description TEXT NOT NULL DEFAULT '',
    base_price_cents INT8 NOT NULL CHECK (base_price_cents >= 0),
    weight_oz INT8 NOT NULL CHECK (weight_oz >= 0),
    category TEXT,
    inventory_count INT8 NOT NULL DEFAULT 0,
    is_active BOOLEAN NOT NULL DEFAULT true,
    sort_order INT8 NOT NULL DEFAULT 0,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_products_is_active ON products(is_active);

COMMENT ON TABLE products IS 'Store products with base pricing and inventory';

--------------------------------------------------------------------------------
-- Product Images

CREATE TABLE product_images (
    id SERIAL8 PRIMARY KEY,
    product_id INT8 NOT NULL REFERENCES products(id) ON DELETE CASCADE,
    image_path TEXT NOT NULL,
    alt_text TEXT NOT NULL DEFAULT '',
    sort_order INT8 NOT NULL DEFAULT 0,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_product_images_product_id ON product_images(product_id);

COMMENT ON TABLE product_images IS 'Product images ordered by sort_order; first is hero/thumbnail';

--------------------------------------------------------------------------------
-- Product Option Types (e.g., "Size", "Color")

CREATE TABLE product_option_types (
    id SERIAL8 PRIMARY KEY,
    product_id INT8 NOT NULL REFERENCES products(id) ON DELETE CASCADE,
    name TEXT NOT NULL,
    sort_order INT8 NOT NULL DEFAULT 0
);

CREATE INDEX idx_product_option_types_product_id ON product_option_types(product_id);

COMMENT ON TABLE product_option_types IS 'Product option dimensions like Size, Color';

--------------------------------------------------------------------------------
-- Product Option Values (e.g., "S", "M", "L")

CREATE TABLE product_option_values (
    id SERIAL8 PRIMARY KEY,
    option_type_id INT8 NOT NULL REFERENCES product_option_types(id) ON DELETE CASCADE,
    value TEXT NOT NULL,
    sort_order INT8 NOT NULL DEFAULT 0
);

CREATE INDEX idx_product_option_values_option_type_id ON product_option_values(option_type_id);

COMMENT ON TABLE product_option_values IS 'Individual values for product option types';

--------------------------------------------------------------------------------
-- Product Variants (e.g., "M / Black")

CREATE TABLE product_variants (
    id SERIAL8 PRIMARY KEY,
    product_id INT8 NOT NULL REFERENCES products(id) ON DELETE CASCADE,
    label TEXT NOT NULL,
    price_cents INT8 CHECK (price_cents >= 0),
    inventory_count INT8 NOT NULL DEFAULT 0,
    sku TEXT,
    weight_oz INT8 CHECK (weight_oz >= 0),
    sort_order INT8 NOT NULL DEFAULT 0,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    deleted_at TIMESTAMPTZ
);

CREATE INDEX idx_product_variants_product_id ON product_variants(product_id);
CREATE INDEX idx_product_variants_deleted_at ON product_variants(deleted_at);

COMMENT ON TABLE product_variants IS 'Product variants with optional price/weight overrides and per-variant inventory. deleted_at enables soft delete for order history FK integrity.';

-- View that replaces product-level inventory with the sum of active variant
-- inventories when variants exist. Used by all product queries.
CREATE VIEW products_with_inventory AS
  SELECT
    p.id, p.name, p.slug, p.description, p.base_price_cents,
    p.weight_oz, p.category,
    COALESCE(v.total_inventory, p.inventory_count)::int8 AS inventory_count,
    p.is_active, p.sort_order, p.created_at, p.updated_at
  FROM products p
  LEFT JOIN (
    SELECT product_id, SUM(inventory_count)::int8 AS total_inventory
    FROM product_variants
    WHERE deleted_at IS NULL
    GROUP BY product_id
  ) v ON v.product_id = p.id;

--------------------------------------------------------------------------------
-- Product Variant Options (join: variant <-> option value)

CREATE TABLE product_variant_options (
    variant_id INT8 NOT NULL REFERENCES product_variants(id) ON DELETE CASCADE,
    option_value_id INT8 NOT NULL REFERENCES product_option_values(id) ON DELETE CASCADE,
    PRIMARY KEY (variant_id, option_value_id)
);

COMMENT ON TABLE product_variant_options IS 'Maps variants to their selected option values';

--------------------------------------------------------------------------------
-- Orders

CREATE TABLE orders (
    id SERIAL8 PRIMARY KEY,
    order_number TEXT NOT NULL UNIQUE,
    email TEXT NOT NULL,
    status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending', 'paid', 'shipped', 'completed', 'cancelled', 'refunded')),
    shipping_first_name TEXT NOT NULL,
    shipping_last_name TEXT NOT NULL,
    shipping_address_line1 TEXT NOT NULL,
    shipping_address_line2 TEXT NOT NULL DEFAULT '',
    shipping_city TEXT NOT NULL,
    shipping_state TEXT NOT NULL,
    shipping_zip TEXT NOT NULL,
    shipping_country TEXT NOT NULL DEFAULT 'US',
    shipping_method TEXT NOT NULL,
    subtotal_cents INT8 NOT NULL CHECK (subtotal_cents >= 0),
    shipping_cents INT8 NOT NULL CHECK (shipping_cents >= 0),
    tax_cents INT8 NOT NULL CHECK (tax_cents >= 0),
    total_cents INT8 NOT NULL CHECK (total_cents >= 0),
    stripe_payment_intent_id TEXT,
    paypal_order_id TEXT,
    payment_method TEXT NOT NULL CHECK (payment_method IN ('stripe', 'paypal')),
    easypost_shipment_id TEXT,
    tracking_number TEXT,
    label_url TEXT,
    notes TEXT NOT NULL DEFAULT '',
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_orders_status ON orders(status);
CREATE INDEX idx_orders_email ON orders(email);

COMMENT ON TABLE orders IS 'Store orders with shipping and payment info';

--------------------------------------------------------------------------------
-- Order Items

CREATE TABLE order_items (
    id SERIAL8 PRIMARY KEY,
    order_id INT8 NOT NULL REFERENCES orders(id) ON DELETE CASCADE,
    -- RESTRICT (default): products and variants referenced by orders cannot be
    -- hard-deleted. Products are deactivated instead; variants use soft-delete.
    product_id INT8 NOT NULL REFERENCES products(id) ON DELETE RESTRICT,
    variant_id INT8 REFERENCES product_variants(id) ON DELETE RESTRICT,
    product_name TEXT NOT NULL,
    variant_label TEXT NOT NULL DEFAULT '',
    quantity INT8 NOT NULL CHECK (quantity > 0),
    unit_price_cents INT8 NOT NULL CHECK (unit_price_cents >= 0),
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_order_items_order_id ON order_items(order_id);

COMMENT ON TABLE order_items IS 'Snapshot of items at time of purchase';

--------------------------------------------------------------------------------
-- Store Settings (single-row)

CREATE TABLE store_settings (
    id INT8 NOT NULL DEFAULT 1 CHECK (id = 1) PRIMARY KEY,
    tax_rate percentage NOT NULL DEFAULT 0.095,
    ship_from_name TEXT NOT NULL DEFAULT 'KPBJ 95.9FM',
    ship_from_address_line1 TEXT NOT NULL DEFAULT '',
    ship_from_city TEXT NOT NULL DEFAULT '',
    ship_from_state TEXT NOT NULL DEFAULT 'CA',
    ship_from_zip TEXT NOT NULL DEFAULT '',
    ship_from_country TEXT NOT NULL DEFAULT 'US',
    order_notification_email TEXT NOT NULL DEFAULT '',
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

INSERT INTO store_settings (id) VALUES (1) ON CONFLICT DO NOTHING;

COMMENT ON TABLE store_settings IS 'Single-row store configuration for tax rate and shipping origin';
