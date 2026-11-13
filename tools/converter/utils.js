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

/** @param {string} hexColor */
export function getLuminance(hexColor) {
  const color = normalizeColor(hexColor);
  const [r, g, b] = [1, 3, 5].map((i) => {
    const channel = parseInt(color.slice(i, i + 2), 16) / 255;
    return channel <= 0.04045
      ? channel / 12.92
      : ((channel + 0.055) / 1.055) ** 2.4;
  });
  return 0.2126 * r + 0.7152 * g + 0.0722 * b;
}

/**
 * @param {string} color
 * @param {string} target
 * @param {number} amount
 */
function mix(color, target, amount) {
  const from = normalizeColor(color);
  const to = normalizeColor(target);
  return `#${[1, 3, 5]
    .map((i) => {
      const a = parseInt(from.slice(i, i + 2), 16);
      const b = parseInt(to.slice(i, i + 2), 16);
      return Math.round(a + (b - a) * amount)
        .toString(16)
        .padStart(2, "0");
    })
    .join("")}`;
}

/**
 * @param {string} hexColor
 * @param {number} factor
 */
export function adjustColorLuminance(hexColor, factor) {
  if (!Number.isFinite(factor) || factor < -1 || factor > 1) {
    throw new RangeError(`Invalid luminance factor: ${factor}`);
  }
  return mix(hexColor, factor < 0 ? "#000000" : "#ffffff", Math.abs(factor));
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
  if (!Number.isFinite(minContrast) || minContrast < 1 || minContrast > 21) {
    throw new RangeError(`Invalid contrast ratio: ${minContrast}`);
  }
  const fg = normalizeColor(foreground);
  const bg = normalizeColor(background);
  if (getContrastRatio(fg, bg) >= minContrast) return fg;
  const target =
    getContrastRatio("#ffffff", bg) > getContrastRatio("#000000", bg)
      ? "#ffffff"
      : "#000000";
  if (getContrastRatio(target, bg) < minContrast) {
    throw new RangeError(
      `Contrast ratio ${minContrast} is unattainable against ${bg}`,
    );
  }
  let low = 0;
  let high = 1;
  for (let i = 0; i < 24; i++) {
    const mid = (low + high) / 2;
    if (getContrastRatio(mix(fg, target, mid), bg) >= minContrast) high = mid;
    else low = mid;
  }
  return mix(fg, target, high);
}

/**
 * @param {import("fs").PathLike | fs.FileHandle} filePath
 */
export async function readEmacsTheme(filePath) {
  const data = await fs.readFile(filePath, "utf8");
  return JSON.parse(data);
}
