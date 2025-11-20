#!/usr/bin/env python3
"""
Analyze Activity Bar contrast issues in VS Code themes.
This script checks color contrast ratios for the Activity Bar.
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
    """Analyze a single theme for Activity Bar contrast issues."""
    try:
        with open(theme_path, 'r') as f:
            theme = json.load(f)
    except:
        return None
    
    colors = theme.get('colors', {})
    
    # Activity Bar related colors
    activity_bar_bg = colors.get('activityBar.background')
    activity_bar_fg = colors.get('activityBar.foreground')
    activity_bar_inactive_fg = colors.get('activityBar.inactiveForeground')
    activity_bar_active_border = colors.get('activityBar.activeBorder')
    activity_bar_active_bg = colors.get('activityBar.activeBackground')
    
    # Fallback colors
    editor_fg = colors.get('editor.foreground')
    
    issues = []
    
    # Check Activity Bar foreground (active icons) contrast with background
    if activity_bar_bg and activity_bar_fg:
        contrast = calculate_contrast_ratio(activity_bar_bg, activity_bar_fg)
        level = get_wcag_level(contrast)
        
        if contrast and contrast < 4.5:
            issues.append({
                'type': 'activity_bar_icon_contrast',
                'element': 'Active Icons (foreground)',
                'background': activity_bar_bg,
                'foreground': activity_bar_fg,
                'contrast_ratio': round(contrast, 2),
                'wcag_level': level,
                'severity': 'HIGH' if contrast < 3 else 'MEDIUM',
                'description': f'Activity Bar background ({activity_bar_bg}) vs foreground icons ({activity_bar_fg})'
            })
    
    # Check Activity Bar inactive icon contrast with background
    if activity_bar_bg and activity_bar_inactive_fg:
        contrast = calculate_contrast_ratio(activity_bar_bg, activity_bar_inactive_fg)
        level = get_wcag_level(contrast)
        
        if contrast and contrast < 3:  # Much lower threshold for inactive elements
            issues.append({
                'type': 'activity_bar_inactive_contrast',
                'element': 'Inactive Icons (inactiveForeground)',
                'background': activity_bar_bg,
                'foreground': activity_bar_inactive_fg,
                'contrast_ratio': round(contrast, 2),
                'wcag_level': level,
                'severity': 'HIGH',
                'description': f'Activity Bar background ({activity_bar_bg}) vs inactive icons ({activity_bar_inactive_fg})'
            })
    
    # Check Activity Bar background vs editor foreground (if no specific activity bar foreground)
    if activity_bar_bg and not activity_bar_fg and editor_fg:
        contrast = calculate_contrast_ratio(activity_bar_bg, editor_fg)
        level = get_wcag_level(contrast)
        
        if contrast and contrast < 4.5:
            issues.append({
                'type': 'activity_bar_fallback_contrast',
                'element': 'Icons (using editor.foreground)',
                'background': activity_bar_bg,
                'foreground': editor_fg,
                'contrast_ratio': round(contrast, 2),
                'wcag_level': level,
                'severity': 'HIGH' if contrast < 3 else 'MEDIUM',
                'description': f'Activity Bar background ({activity_bar_bg}) vs editor foreground ({editor_fg})'
            })
    
    return {
        'name': theme.get('name', os.path.basename(theme_path)),
        'type': theme.get('type', 'unknown'),
        'colors': {
            'activityBar.background': activity_bar_bg,
            'activityBar.foreground': activity_bar_fg,
            'activityBar.inactiveForeground': activity_bar_inactive_fg,
            'activityBar.activeBorder': activity_bar_active_border,
            'activityBar.activeBackground': activity_bar_active_bg,
            'editor.foreground': editor_fg,
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
    report.append("# VS Code Theme Activity Bar Contrast Analysis Report")
    report.append(f"\nGenerated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    report.append(f"\nTotal themes analyzed: {len(results)}")
    report.append(f"Themes with Activity Bar contrast issues: {len(problematic_themes)}")
    
    if problematic_themes:
        report.append("\n## Themes with Low Activity Bar Contrast Issues\n")
        
        for theme in sorted(problematic_themes, key=lambda x: min([i.get('contrast_ratio', 999) for i in x['issues']], default=999)):
            report.append(f"\n### {theme['name']} ({theme['type']})")
            report.append(f"\nColors configured:")
            for key, value in theme['colors'].items():
                if value:
                    report.append(f"  - `{key}`: {value}")
            
            if theme['issues']:
                report.append(f"\n**Issues found: {len(theme['issues'])}**\n")
                for issue in sorted(theme['issues'], key=lambda x: x.get('contrast_ratio', 999)):
                    report.append(f"- **Element**: {issue['element']}")
                    report.append(f"  - **Contrast Ratio**: {issue['contrast_ratio']}:1 (WCAG {issue['wcag_level']}) [{issue['severity']}]")
                    report.append(f"  - **Background**: {issue['background']}")
                    report.append(f"  - **Foreground**: {issue['foreground']}")
                    report.append("")
    else:
        report.append("\n## ✓ Good News!\n")
        report.append("All themes have acceptable Activity Bar contrast ratios.")
        report.append("Activity Bar icons should be clearly visible in all themes.")
    
    # Summary and recommendations
    report.append("\n## Summary and Recommendations\n")
    
    # Count severity levels
    high_severity = sum(1 for t in problematic_themes for i in t['issues'] if i['severity'] == 'HIGH')
    medium_severity = sum(1 for t in problematic_themes for i in t['issues'] if i['severity'] == 'MEDIUM')
    
    report.append(f"### Key Findings:\n")
    report.append(f"- High severity issues (contrast < 3:1): {high_severity}")
    report.append(f"- Medium severity issues (contrast 3-4.5:1): {medium_severity}")
    
    report.append("\n### WCAG Contrast Standards:\n")
    report.append("- **AAA (7:1)**: Highest accessibility - recommended for all icons")
    report.append("- **AA (4.5:1)**: Enhanced accessibility - minimum for active UI elements")
    report.append("- **A (3:1)**: Minimum for inactive UI elements")
    report.append("- **Below 3:1**: Fails accessibility - icons may be hard to distinguish")
    
    report.append("\n### Activity Bar Context:\n")
    report.append("The Activity Bar is the narrow icon strip on the far left of VS Code.")
    report.append("Icons need sufficient contrast for users to distinguish them easily.")
    report.append("- **Active icons**: Currently selected activity (Explorer, Search, Git, etc.)")
    report.append("- **Inactive icons**: Available but not selected activities")
    report.append("- **Focus indication**: Active border color should provide clear feedback")
    
    if problematic_themes:
        report.append("\n### Recommended Actions:\n")
        report.append("1. **Check if `activityBar.foreground` is explicitly set**")
        report.append("   - If missing, Activity Bar icons may fall back to editor colors")
        report.append("2. **Ensure sufficient contrast** between `activityBar.background` and `activityBar.foreground`")
        report.append("3. **Verify `activityBar.inactiveForeground`** provides enough visual distinction")
        report.append("4. **Test in VS Code** to visually confirm icon visibility")
    
    # Write report to file
    report_dir = Path('/home/meng/git/emacs-theme-to-vscode/logs/ux-reports')
    report_dir.mkdir(parents=True, exist_ok=True)
    
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    report_file = report_dir / f'activity_bar_contrast_analysis_{timestamp}.md'
    
    with open(report_file, 'w') as f:
        f.write('\n'.join(report))
    
    print(f"\n✓ Report generated: {report_file}")
    print(f"\nFound {len(problematic_themes)} themes with Activity Bar contrast issues")
    
    return problematic_themes

if __name__ == '__main__':
    problematic_themes = main()
    
    # Print summary to console
    if problematic_themes:
        print("\n" + "="*60)
        print("THEMES WITH ACTIVITY BAR CONTRAST ISSUES:")
        print("="*60)
        for theme in problematic_themes[:15]:  # Show first 15
            print(f"\n{theme['name']} ({theme['type']}):")
            for issue in theme['issues']:
                print(f"  {issue['element']}: {issue['contrast_ratio']}:1 (WCAG {issue['wcag_level']}) - {issue['severity']}")
        if len(problematic_themes) > 15:
            print(f"\n... and {len(problematic_themes) - 15} more themes")
    else:
        print("\n✓ All themes have acceptable Activity Bar contrast!")
