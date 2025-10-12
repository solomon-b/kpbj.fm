module.exports = {
  content: [
    './*.html',
    './components/**/*.html',
    './assets/js/**/*.js',  // or wherever your JS is
  ],
  theme: {
    extend: {
      fontFamily: {
        body: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'],
      },
    },
  },
  plugins: [],
}
