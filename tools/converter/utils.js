// @ts-check

import fs from "node:fs/promises";
import { NAMED_COLORS } from "./namedColors.js";

/** @param {string} color */
export function normalizeColor(color) {
  if (typeof color !== "string" || !color) {
    throw new Error(`Invalid color: ${color}`);
  }
  const lower = color.toLowerCase();
  const value = (NAMED_COLORS[lower] ?? lower).toLowerCase();
  if (/^color-\d+$/.test(value)) {
    const index = Number(value.slice(6));
    if (index > 255) throw new RangeError(`Invalid xterm color: ${color}`);
    const ansi = [
      "#000000",
      "#800000",
      "#008000",
      "#808000",
      "#000080",
      "#800080",
      "#008080",
      "#c0c0c0",
      "#808080",
      "#ff0000",
      "#00ff00",
      "#ffff00",
      "#0000ff",
      "#ff00ff",
      "#00ffff",
      "#ffffff",
    ];
    if (index < 16) return ansi[index];
    const levels = [0, 95, 135, 175, 215, 255];
    const channels =
      index < 232
        ? [
            levels[Math.floor((index - 16) / 36)],
            levels[Math.floor((index - 16) / 6) % 6],
            levels[(index - 16) % 6],
          ]
        : Array(3).fill(8 + 10 * (index - 232));
    return `#${channels.map((channel) => channel.toString(16).padStart(2, "0")).join("")}`;
  }
  if (/^#[0-9a-f]{3}$/.test(value)) {
    return `#${[...value.slice(1)].map((digit) => digit + digit).join("")}`;
  }
  if (/^#[0-9a-f]{6}([0-9a-f]{2})?$/.test(value)) {
    return value.slice(0, 7);
  }
  if (/^#[0-9a-f]{12}$/.test(value)) {
    return `#${[1, 5, 9]
      .map((i) =>
        Math.round(parseInt(value.slice(i, i + 4), 16) / 257)
          .toString(16)
          .padStart(2, "0"),
      )
      .join("")}`;
  }
  throw new Error(`Unknown or invalid color: ${color}`);
}

/**
 * @param {string} color
 * @param {number} alpha
 */
export function withAlpha(color, alpha) {
  if (!Number.isFinite(alpha) || alpha < 0 || alpha > 1) {
    throw new RangeError(`Invalid alpha: ${alpha}`);
  }
  return (
    normalizeColor(color) +
    Math.round(alpha * 255)
      .toString(16)
      .padStart(2, "0")
  );
}

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
 * @param {string} background
 * @param {number} minContrast
 */
export function adjustColorForContrast(
  foreground,
  background,
  minContrast = 4.5,
) {
  // Check current contrast
  const currentContrast = getContrastRatio(foreground, background);
  if (currentContrast >= minContrast) {
    return foreground;
  }

  // Need to adjust - make foreground more extreme (lighter or darker)
  const fgLum = getLuminance(foreground);
  const bgLum = getLuminance(background);

  // If background is light, darken foreground; if dark, lighten it
  const shouldDarken = bgLum > 0.5;

  // Try adjustments incrementally
  for (let factor = 0.1; factor <= 0.5; factor += 0.05) {
    const adjusted = shouldDarken
      ? adjustColorLuminance(foreground, -factor)
      : adjustColorLuminance(foreground, factor);
    const contrast = getContrastRatio(adjusted, background);
    if (contrast >= minContrast) {
      return adjusted;
    }
  }

  // If we still can't reach minContrast, return the most extreme adjustment
  return shouldDarken
    ? adjustColorLuminance(foreground, -0.5)
    : adjustColorLuminance(foreground, 0.5);
}

/**
 * @param {import("fs").PathLike | fs.FileHandle} filePath
 */
export async function readEmacsTheme(filePath) {
  const data = await fs.readFile(filePath, "utf8");
  return JSON.parse(data);
}
