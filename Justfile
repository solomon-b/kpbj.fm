default:
    just build

build:
    rm -rf dist
    mkdir -p dist/assets/css dist/assets/img dist/assets/text dist/pages
    cp -r assets/img/* dist/assets/img/
    cp -r assets/css/* dist/assets/css/
    cp -r assets/text/* dist/assets/text/
    just build-html
    just build-pages
    just tailwind-build

build-html:
    #!/usr/bin/env bash
    find src -name "*.html" -type f | while read -r src_file; do
        # Get relative path from src/
        rel_path="${src_file#src/}"
        # Create output path in dist/
        output_file="dist/$rel_path"
        # Create directory if it doesn't exist
        mkdir -p "$(dirname "$output_file")"
        # Process the file
        just inject-components "$src_file" > "$output_file"
    done

inject-components src:
    MAIN_CONTENT="$(cat {{src}})" envsubst '${MAIN_CONTENT}' < components/page-template.html

build-pages:
    #!/usr/bin/env bash
    # Copy raw HTML content to dist/pages for Alpine.js to fetch
    cp src/index.html dist/pages/home.html
    cp src/about-us/index.html dist/pages/about-us.html
    cp src/donate/index.html dist/pages/donate.html

dev:
    concurrently --kill-others --names "watch,serve" \
        "just watch" \
        "serve dist"

serve:
    serve dist

tailwind-build:
    mkdir -p dist/assets/css
    tailwindcss -i ./assets/css/tailwind.css -o dist/assets/css/output.css --minify --content "dist/**/*.html"

watch:
    just list-watched-files | entr -r just build

list-watched-files:
    echo index.html; fd . --extension html --extension css --extension js --extension png components assets
