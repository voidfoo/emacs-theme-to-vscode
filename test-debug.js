const fs = require("fs");
const path = require("path");

// Require the convert module (which will run the conversion)
// But first, let me just manually test the function

const NAMED_COLORS = require("./tools/converter/utils.js").NAMED_COLORS || {};

// Read the Emacs theme
const themeData = JSON.parse(
  fs.readFileSync("emacs-definitions/emacs-doom-Iosvkem.json", "utf8")
);

console.log("Emacs mode-line:", themeData["mode-line"]);
console.log("mode-line.fg:", themeData["mode-line"].fg);
console.log("mode-line.bg:", themeData["mode-line"].bg);

// Check what gets set
if (!themeData["mode-line"].fg) {
  console.log("mode-line does NOT have fg defined");
} else {
  console.log("mode-line HAS fg defined:", themeData["mode-line"].fg);
}
