#!/usr/bin/env python3
"""
Analyze sidebar contrast issues in VS Code themes.
This script checks color contrast ratios between sidebar backgrounds and text colors.
"""

import json
import os
from pathlib import Path
from datetime import datetime

def hex_to_rgb(hex_color):
    """Convert hex color to RGB tuple."""
    hex_color = hex_color.lstrip('#')
    return tuple(int(hex_color[i:i+2], 16) for i in (0, 2, 4))

def rgb_to_luminance(rgb):
    """Calculate relative luminance using WCAG formula."""
    r, g, b = [x / 255.0 for x in rgb]
    
    # Apply gamma correction
    r = r / 12.92 if r <= 0.03928 else ((r + 0.055) / 1.055) ** 2.4
    g = g / 12.92 if g <= 0.03928 else ((g + 0.055) / 1.055) ** 2.4
    b = b / 12.92 if b <= 0.03928 else ((b + 0.055) / 1.055) ** 2.4
    
    return 0.2126 * r + 0.7152 * g + 0.0722 * b

def calculate_contrast_ratio(color1, color2):
    """Calculate WCAG contrast ratio between two colors."""
    try:
        rgb1 = hex_to_rgb(color1)
        rgb2 = hex_to_rgb(color2)
        
        lum1 = rgb_to_luminance(rgb1)
        lum2 = rgb_to_luminance(rgb2)
        
        lighter = max(lum1, lum2)
        darker = min(lum1, lum2)
        
        return (lighter + 0.05) / (darker + 0.05)
    except:
        return None

def get_wcag_level(contrast_ratio):
    """Get WCAG compliance level."""
    if contrast_ratio is None:
        return "ERROR"
    if contrast_ratio >= 7:
        return "AAA"
    elif contrast_ratio >= 4.5:
        return "AA"
    elif contrast_ratio >= 3:
        return "A (large text)"
    else:
        return "FAIL"

def analyze_theme(theme_path):
    """Analyze a single theme for contrast issues."""
    try:
        with open(theme_path, 'r') as f:
            theme = json.load(f)
    except:
        return None
    
    colors = theme.get('colors', {})
    
    # Key sidebar-related colors to check
    sidebar_bg = colors.get('sideBar.background')
    activity_bar_bg = colors.get('activityBar.background')
    editor_fg = colors.get('editor.foreground')
    list_foreground = colors.get('list.foreground')
    list_hover_bg = colors.get('list.hoverBackground')
    list_active_selection_bg = colors.get('list.activeSelectionBackground')
    
    issues = []
    
    # Check main sidebar text contrast
    if sidebar_bg and editor_fg:
        contrast = calculate_contrast_ratio(sidebar_bg, editor_fg)
        level = get_wcag_level(contrast)
        
        if contrast and contrast < 4.5:
            issues.append({
                'type': 'sidebar_text_contrast',
                'background': sidebar_bg,
                'foreground': editor_fg,
                'contrast_ratio': round(contrast, 2),
                'wcag_level': level,
                'severity': 'HIGH' if contrast < 3 else 'MEDIUM',
                'description': f'Sidebar background ({sidebar_bg}) vs editor foreground ({editor_fg})'
            })
    
    # Check sidebar with list foreground if available
    if sidebar_bg and list_foreground:
        contrast = calculate_contrast_ratio(sidebar_bg, list_foreground)
        level = get_wcag_level(contrast)
        
        if contrast and contrast < 4.5:
            issues.append({
                'type': 'sidebar_list_contrast',
                'background': sidebar_bg,
                'foreground': list_foreground,
                'contrast_ratio': round(contrast, 2),
                'wcag_level': level,
                'severity': 'HIGH' if contrast < 3 else 'MEDIUM',
                'description': f'Sidebar background ({sidebar_bg}) vs list foreground ({list_foreground})'
            })
    
    return {
        'name': theme.get('name', os.path.basename(theme_path)),
        'type': theme.get('type', 'unknown'),
        'colors': {
            'sideBar.background': sidebar_bg,
            'activityBar.background': activity_bar_bg,
            'editor.foreground': editor_fg,
            'list.foreground': list_foreground,
            'list.hoverBackground': list_hover_bg,
            'list.activeSelectionBackground': list_active_selection_bg,
        },
        'issues': issues
    }

def main():
    themes_dir = Path('/home/meng/git/emacs-theme-to-vscode/vscode-extension/themes')
    
    # Analyze all themes
    results = []
    for theme_file in sorted(themes_dir.glob('*.json')):
        result = analyze_theme(theme_file)
        if result:
            results.append(result)
    
    # Filter themes with issues
    problematic_themes = [r for r in results if r['issues']]
    
    # Generate report
    report = []
    report.append("# VS Code Theme Sidebar Contrast Analysis Report")
    report.append(f"\nGenerated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    report.append(f"\nTotal themes analyzed: {len(results)}")
    report.append(f"Themes with contrast issues: {len(problematic_themes)}")
    
    if problematic_themes:
        report.append("\n## Themes with Low Sidebar Contrast Issues\n")
        
        for theme in sorted(problematic_themes, key=lambda x: min([i.get('contrast_ratio', 999) for i in x['issues']], default=999)):
            report.append(f"\n### {theme['name']} ({theme['type']})")
            report.append(f"\nColors configured:")
            for key, value in theme['colors'].items():
                if value:
                    report.append(f"  - `{key}`: {value}")
            
            if theme['issues']:
                report.append(f"\n**Issues found: {len(theme['issues'])}**\n")
                for issue in sorted(theme['issues'], key=lambda x: x.get('contrast_ratio', 999)):
                    report.append(f"- **Contrast Ratio**: {issue['contrast_ratio']}:1 (WCAG {issue['wcag_level']}) [{issue['severity']}]")
                    report.append(f"  - {issue['description']}")
                    report.append("")
    
    # Summary and recommendations
    report.append("\n## Summary and Recommendations\n")
    report.append("### Key Findings:\n")
    
    # Count severity levels
    high_severity = sum(1 for t in problematic_themes for i in t['issues'] if i['severity'] == 'HIGH')
    medium_severity = sum(1 for t in problematic_themes for i in t['issues'] if i['severity'] == 'MEDIUM')
    
    report.append(f"- High severity issues (contrast < 3:1): {high_severity}")
    report.append(f"- Medium severity issues (contrast 3-4.5:1): {medium_severity}")
    
    report.append("\n### WCAG Contrast Standards:\n")
    report.append("- **AAA (7:1)**: Highest accessibility - recommended for all text")
    report.append("- **AA (4.5:1)**: Enhanced accessibility - minimum for most purposes")
    report.append("- **A (3:1)**: Minimum for large text only")
    report.append("- **Below 3:1**: Fails all WCAG standards")
    
    report.append("\n### Recommended Actions:\n")
    report.append("1. **Update sidebar colors** to have minimum 4.5:1 contrast ratio")
    report.append("2. **Verify list.foreground** is explicitly set in themes without it")
    report.append("3. **Review inactive selection colors** which also affect readability")
    report.append("4. **Test in VS Code** to visually confirm improvements")
    
    report.append("\n### Color Adjustment Guide:\n")
    report.append("To fix low contrast issues:")
    report.append("- Either lighten the foreground color (text)")
    report.append("- Or darken the background color")
    report.append("- Aim for at least 4.5:1 ratio for normal text (AA standard)")
    
    # Write report to file
    report_dir = Path('/home/meng/git/emacs-theme-to-vscode/logs/ux-reports')
    report_dir.mkdir(parents=True, exist_ok=True)
    
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    report_file = report_dir / f'sidebar_contrast_analysis_{timestamp}.md'
    
    with open(report_file, 'w') as f:
        f.write('\n'.join(report))
    
    print(f"\n✓ Report generated: {report_file}")
    print(f"\nFound {len(problematic_themes)} themes with sidebar contrast issues")
    
    return problematic_themes

if __name__ == '__main__':
    problematic_themes = main()
    
    # Print summary to console
    if problematic_themes:
        print("\n" + "="*60)
        print("THEMES WITH SIDEBAR CONTRAST ISSUES:")
        print("="*60)
        for theme in problematic_themes[:10]:  # Show first 10
            print(f"\n{theme['name']} ({theme['type']}):")
            for issue in theme['issues']:
                print(f"  Contrast: {issue['contrast_ratio']}:1 (WCAG {issue['wcag_level']}) - {issue['severity']}")
        if len(problematic_themes) > 10:
            print(f"\n... and {len(problematic_themes) - 10} more themes")
