#!/usr/bin/env python3

import json
import os
import sys
import urllib.request
from pathlib import Path

def load_sources():
    sources_path = Path(__file__).parent.parent.parent / 'emacs-themes' / 'sources.json'
    with open(sources_path) as f:
        return json.load(f)

def download_theme_file(url, save_path):
    try:
        print(f"Trying to download from {url}")  # Debug log
        with urllib.request.urlopen(url) as response:
            content = response.read().decode('utf-8')
        
        save_path.parent.mkdir(parents=True, exist_ok=True)
        with open(save_path, 'w') as f:
            f.write(content)
        print(f"Downloaded {url} to {save_path}")
        return True
    except Exception as e:
        print(f"Error downloading {url}: {e}", file=sys.stderr)
        return False

def main():
    sources = load_sources()
    emacs_themes_dir = Path(__file__).parent.parent.parent / 'emacs-themes'
    
    for theme_name, repo_url in sources.items():
        theme_dir = emacs_themes_dir / theme_name
        
        def get_github_raw_url(repo_url, file_path, branch='master'):
            # If the URL is already a raw GitHub URL, use it directly
            if repo_url.startswith('https://raw.githubusercontent.com/'):
                return f"{repo_url}/{file_path}"
            # Otherwise convert https://github.com/user/repo to https://raw.githubusercontent.com/user/repo/branch
            parts = repo_url.replace('https://github.com/', '').split('/')
            return f"https://raw.githubusercontent.com/{parts[0]}/{parts[1]}/{branch}/{file_path}"

        # Special-cases for known packages
        if theme_name == 'spacemacs':
            files = [
                'spacemacs-theme.el',
                'spacemacs-dark-theme.el',
                'spacemacs-light-theme.el'
            ]
            for file in files:
                url = get_github_raw_url(repo_url, file)
                save_path = theme_dir / file
                download_theme_file(url, save_path)

        # Doom themes are stored under a themes/ subdirectory and include many files
        elif theme_name == 'doom':
            # Try to download the entire themes/ directory index is not available via raw, so
            # attempt to fetch a known list of common theme files present in the repo README.
            # We'll try to download files from the 'themes/' directory and fall back gracefully.
            common_files = [
                'themes/doom-one-theme.el',
                'themes/doom-dracula-theme.el',
                'themes/doom-monokai-pro-theme.el',
                'themes/doom-peacock-theme.el',
                'themes/doom-ayu-dark-theme.el'
            ]
            for file in common_files:
                url = get_github_raw_url(repo_url, file)
                save_path = theme_dir / file.split('/')[-1]
                download_theme_file(url, save_path)

        # Handle single theme files (default)
        else:
            # Try both main and master branches, and different file paths
            file_name = "leuven-theme.el" if theme_name == "leuven-theme" else f"{theme_name}-theme.el"
            paths_to_try = [
                (file_name, 'master'),
                (file_name, 'main'),
                (f"src/{file_name}", 'master'),
                (f"src/{file_name}", 'main'),
                (file_name.replace('-theme.el', '.el'), 'master'),  # Try without -theme suffix
                (file_name.replace('-theme.el', '.el'), 'main')
            ]
            
            downloaded = False
            for path, branch in paths_to_try:
                url = get_github_raw_url(repo_url, path, branch)
                save_path = theme_dir / file_name
                if download_theme_file(url, save_path):
                    downloaded = True
                    break
                    
            if not downloaded:
                print(f"Error: Could not download {file_name} from any attempted location", file=sys.stderr)
            save_path = theme_dir / file_name
            download_theme_file(url, save_path)

if __name__ == '__main__':
    main()