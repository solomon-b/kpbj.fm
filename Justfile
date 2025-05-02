# Run the development server
serve:
    serve

# Build Tailwind CSS
tailwind-build:
    tailwindcss -i ./assets/css/tailwind.css -o ./assets/css/output.css --minify
