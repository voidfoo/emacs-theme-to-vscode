// @ts-check

import fs from "node:fs/promises";

// Helper function to calculate luminance
/**
 * @param {string} hexColor
 */
export function getLuminance(hexColor) {
  const rgb = hexColor.match(/^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i);
  if (!rgb) return 0;
  const [r, g, b] = rgb.slice(1).map((x) => parseInt(x, 16) / 255);
  return 0.2126 * r + 0.7152 * g + 0.0722 * b;
}

/**
 * @param {string} hexColor
 * @param {number} factor
 */
export function adjustColorLuminance(hexColor, factor) {
  const rgb = hexColor.match(/^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i);
  if (!rgb) return hexColor;

  const [r, g, b] = rgb.slice(1).map((x) => parseInt(x, 16));
  const adjust = (/** @type {number} */ value) => {
    const newValue = Math.round(value * (1 + factor));
    return Math.min(255, Math.max(0, newValue));
  };

  const newR = adjust(r).toString(16).padStart(2, "0");
  const newG = adjust(g).toString(16).padStart(2, "0");
  const newB = adjust(b).toString(16).padStart(2, "0");

  return `#${newR}${newG}${newB}`;
}

// Helper function to calculate contrast ratio
/**
 * @param {string} color1
 * @param {string} color2
 */
export function getContrastRatio(color1, color2) {
  const l1 = getLuminance(color1);
  const l2 = getLuminance(color2);
  const lighter = Math.max(l1, l2);
  const darker = Math.min(l1, l2);
  return (lighter + 0.05) / (darker + 0.05);
}

// Helper function to adjust color for minimum contrast
/**
 * @param {string} foreground
 * @param {string} _background
 */
export function adjustColorForContrast(
  foreground,
  _background,
  _minContrast = 4.5,
) {
  // Implementation of color adjustment logic
  // This is a placeholder - would need actual color manipulation code
  return foreground;
}

/**
 * @param {import("fs").PathLike | fs.FileHandle} filePath
 */
export async function readEmacsTheme(filePath) {
  const data = await fs.readFile(filePath, "utf8");
  return JSON.parse(data);
}
