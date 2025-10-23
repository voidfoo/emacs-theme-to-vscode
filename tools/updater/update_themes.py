#!/usr/bin/env python3

import json
import os
import sys
import urllib.request
import urllib.error
from pathlib import Path
import ssl
import base64

GITHUB_API = 'https://api.github.com'

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
            # Prefer to enumerate the repository using the GitHub tree API to list all files under themes/
            def parse_github_repo(url):
                # Accept forms like https://github.com/owner/repo or git@github.com:owner/repo.git
                if url.startswith('https://github.com/'):
                    parts = url.replace('https://github.com/', '').strip('/').split('/')
                    if len(parts) >= 2:
                        return parts[0], parts[1].replace('.git','')
                # naive git@ parser
                if url.startswith('git@github.com:'):
                    parts = url.replace('git@github.com:', '').split('/')
                    return parts[0], parts[1].replace('.git','')
                return None, None

            owner, repo = parse_github_repo(repo_url)
            downloaded = False
            if owner and repo:
                # Try to get the default branch via repo API
                try:
                    headers = {'User-Agent': 'emacs-theme-updater/1.0'}
                    token = os.environ.get('GITHUB_TOKEN')
                    if token:
                        headers['Authorization'] = f'token {token}'

                    def api_get(path):
                        req = urllib.request.Request(GITHUB_API + path, headers=headers)
                        with urllib.request.urlopen(req, context=ssl.create_default_context()) as resp:
                            return json.load(resp)

                    repo_info = api_get(f'/repos/{owner}/{repo}')
                    default_branch = repo_info.get('default_branch', 'master')

                    # List tree for the default branch recursively and filter themes/
                    tree_info = api_get(f'/repos/{owner}/{repo}/git/trees/{default_branch}?recursive=1')
                    tree = tree_info.get('tree', [])
                    theme_files = [item['path'] for item in tree if item['type']=='blob' and item['path'].startswith('themes/') and item['path'].endswith('-theme.el')]

                    if theme_files:
                        for path in theme_files:
                            raw_url = get_github_raw_url(repo_url, path, default_branch)
                            save_path = theme_dir / Path(path).name
                            if download_theme_file(raw_url, save_path):
                                downloaded = True
                except urllib.error.HTTPError as e:
                    print(f'GitHub API request failed: {e}', file=sys.stderr)
                except Exception as e:
                    print(f'Error while enumerating Doom themes via API: {e}', file=sys.stderr)

            # If API approach failed, fall back to the older small shortlist
            if not downloaded:
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