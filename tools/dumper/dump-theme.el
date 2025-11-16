;; Theme face dumper for VS Code conversion  -*- lexical-binding: t; -*-
;; Requires Emacs 30+

(require 'json)

(defun dump-theme-disable-all ()
  "Disable all currently enabled themes."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun dump-theme-faces (theme-name output-file)
  "Dump face colors from THEME-NAME to OUTPUT-FILE in JSON format.
THEME-NAME should be a symbol naming an available theme.
OUTPUT-FILE is the path where the JSON will be written.
Disables all currently enabled themes before loading the target theme."
  (interactive
   (let* ((theme-str (completing-read "Theme name: "
                                      (mapcar #'symbol-name (custom-available-themes))))
          (theme-sym (intern theme-str))
          (output-dir (expand-file-name "emacs-definitions"
                                        (locate-dominating-file default-directory ".git")))
          (default-file (expand-file-name (concat "emacs-" theme-str ".json")
                                          output-dir)))
     (list theme-sym
           (read-file-name "Output JSON file: "
                           output-dir
                           default-file
                           nil
                           (file-name-nondirectory default-file)))))

  ;; Store currently enabled themes to restore later
  (let ((old-themes custom-enabled-themes))
    ;; Disable all themes before loading the target
    (dump-theme-disable-all)

    ;; Load the theme temporarily without enabling it globally
    (load-theme theme-name t)

    (let* ((face-attrs (make-hash-table :test 'equal))
           (all-faces (face-list)))

      ;; Collect face attributes
      (dolist (face all-faces)
        (let ((fg (face-attribute face :foreground nil t))
              (bg (face-attribute face :background nil t)))
          ;; Only store faces that have explicit colors
          (when (or (not (eq fg 'unspecified))
                    (not (eq bg 'unspecified)))
            (puthash (symbol-name face)
                     (let ((attrs (make-hash-table :test 'equal)))
                       (unless (eq fg 'unspecified)
                         (puthash "fg" fg attrs))
                       (unless (eq bg 'unspecified)
                         (puthash "bg" bg attrs))
                       attrs)
                     face-attrs))))

      ;; Convert to JSON and write to file
      (with-temp-buffer
        (insert (json-encode face-attrs))
        (json-pretty-print-buffer)
        (write-region (point-min) (point-max) output-file))

      (message "Wrote theme face definitions to %s" output-file))

    ;; Disable our theme and restore previous themes
    (disable-theme theme-name)
    (dolist (theme old-themes)
      (enable-theme theme))))

(defun dump-all-theme-faces ()
  "Export all available themes to JSON files in emacs-definitions directory."
  (interactive)
  (let* ((output-dir (expand-file-name "emacs-definitions"
                                       (locate-dominating-file default-directory ".git")))
         (themes (custom-available-themes))
         (total (length themes))
         (count 0))

    ;; Create output directory if needed
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))

    ;; Store current themes to restore later
    (let ((old-themes custom-enabled-themes))
      ;; Process each theme
      (dolist (theme themes)
        (setq count (1+ count))
        (message "[%d/%d] Processing theme: %s" count total theme)
        (let ((output-file (expand-file-name (format "emacs-%s.json" theme) output-dir)))
          (condition-case err
              (dump-theme-faces theme output-file)
            (error
             (message "Warning: Failed to process theme %s: %s" theme (error-message-string err))
             (sleep-for 1)))))

      ;; Restore original themes
      (dump-theme-disable-all)
      (dolist (theme old-themes)
        (enable-theme theme)))

    (message "Exported %d themes to %s" total output-dir)))

;; Add commands to menu
(easy-menu-add-item nil '("tools")
                    ["Dump Single Theme"
                     (call-interactively #'dump-theme-faces)
                     :help "Export a single theme's face colors to JSON"])

(easy-menu-add-item nil '("tools")
                    ["Dump All Themes"
                     (call-interactively #'dump-all-theme-faces)
                     :help "Export all available themes' face colors to JSON"])

(provide 'dump-theme)
