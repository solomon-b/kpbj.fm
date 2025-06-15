default:
    just build

build:
    rm -rf dist
    mkdir -p dist/assets/css dist/assets/img
    cp -r assets/img/* dist/assets/img/
    cp -r assets/css/* dist/assets/css/
    mkdir dist/donate
    just inject-components src/index.html > dist/index.html
    just inject-components src/donate/index.html > dist/donate/index.html
    just tailwind-build

inject-components src:
    HEADER="$(cat components/header.html)" FOOTER="$(cat components/footer.html)" envsubst < {{src}}

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
