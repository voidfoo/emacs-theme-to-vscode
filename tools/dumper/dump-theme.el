;; Theme face dumper for VS Code conversion
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

;; Add command to menu
(easy-menu-add-item nil '("tools")
                    ["Dump Theme Faces"
                     (call-interactively #'dump-theme-faces)
                     :help "Export a theme's face colors to JSON"])

(provide 'dump-theme)