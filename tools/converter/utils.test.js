// @ts-check
import assert from "node:assert/strict";
import { test } from "node:test";
import { normalizeColor, withAlpha } from "./utils.js";

test("normalizes Emacs and CSS colors before applying alpha", () => {
  for (const value of ["#abc", "#aabbcc", "#aabbccdd"]) {
    assert.equal(normalizeColor(value), "#aabbcc");
    assert.equal(withAlpha(value, 0.5), "#aabbcc80");
  }
  assert.equal(normalizeColor("WHITE"), "#ffffff");
  assert.equal(withAlpha("white", 0.25), "#ffffff40");
  assert.equal(normalizeColor("#22083397778B"), "#223377");
  assert.throws(() => normalizeColor("not-a-color"), /invalid color/i);
  assert.throws(() => withAlpha("#abc", 2), RangeError);
});

test("maps xterm palette indices and rejects invalid indices", () => {
  assert.equal(normalizeColor("color-1"), "#800000");
  assert.equal(normalizeColor("color-16"), "#000000");
  assert.equal(normalizeColor("color-123"), "#87ffff");
  assert.equal(normalizeColor("color-231"), "#ffffff");
  assert.equal(normalizeColor("color-232"), "#080808");
  assert.equal(normalizeColor("color-255"), "#eeeeee");
  assert.throws(() => normalizeColor("color-256"), /xterm/);
  assert.throws(() => normalizeColor("color-1foo"), /invalid color/i);
});
