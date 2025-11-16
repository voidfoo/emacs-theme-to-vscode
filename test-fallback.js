// Test the fallback logic
const statusBarFg = "#dddddd";
const colors = { "statusBar.foreground": statusBarFg };

// Simulating the fallback code
if (
  !colors["statusBarItem.remoteForeground"] &&
  colors["statusBar.foreground"]
) {
  colors["statusBarItem.remoteForeground"] = colors["statusBar.foreground"];
}

console.log("After fallback:");
console.log("colors:", colors);
console.log("JSON.stringify:", JSON.stringify(colors, null, 2));
