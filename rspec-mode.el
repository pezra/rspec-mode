(require 'ruby-mode)

(defconst rspec-mode-abbrev-table (make-abbrev-table))

(defun rspec-keymap ()
  "Creates a keymap for spec files"
  (let ((rspec-mode-map (make-sparse-keymap)))
    (define-keys rspec-mode-map
      ((kbd "C-c ,")   'rspec-verify)
      ((kbd "C-c sd")  'rspec-disable-spec)
      ((kbd "C-c se")  'rspec-enable-spec)
      ((kbd "C-c so")  'rspec-toggle-spec-and-target))
    rspec-mode-map))

(define-minor-mode rspec-mode
  "Minor mode for rSpec files"
  :lighter " rSpec"
  :keymap  (rspec-keymap))


(defun rspec-disable-spec ()
  "Disable the spec in which the point is located"
  (interactive)
  (save-excursion
    (ruby-beginning-of-block)
    (search-forward-regexp "do\\|{")
    (backward-word)
    (insert "\n")
    (let ((start (point)))
      (ruby-end-of-block)
      (search-forward-regexp "end\\|}")
      (comment-region start (point)))))

(defun rspec-enable-spec ()
  "Enable the spec in which the point is located"
  (interactive)
  (save-excursion
    (search-backward-regexp "^[^#]")
    (next-line)
    (join-line)
    (let ((start (point)))
      (search-forward-regexp "^[^#]")
      (uncomment-region start (point)))))

(defun rspec-verify ()
  "Runs the specified spec, or the spec file for the current buffer."
  (interactive)
  (compile (concat ruby-command " " (rspec-spec-file-for (buffer-file-name)) " --format specdoc --reverse")))

(defun rspec-toggle-spec-and-target ()
  "Switches to the spec for the current buffer if it is a
   non-spec file, or switch to the target of the current buffer
   if the current is a spec"
  (interactive)
  (find-file
   (if (rspec-buffer-is-spec-p)
       (rspec-target-file-for (buffer-file-name))
     (rspec-spec-file-for (buffer-file-name)))))



(defun rspec-spec-file-for (a-file-name)
  "Find spec for the specified file"
  (if (rspec-spec-file-p a-file-name)
      a-file-name
    (rspec-specize-file-name (expand-file-name (replace-regexp-in-string "^\\.\\./[^/]+/" "" (file-relative-name a-file-name (rspec-spec-directory a-file-name))) 
                                               (rspec-spec-directory a-file-name)))))

(defun rspec-target-file-for (a-spec-file-name)
  "Find the target for a-spec-file-name"
  (first 
   (file-expand-wildcards 
    (replace-regexp-in-string "/spec/" "/*/" (rspec-targetize-file-name a-spec-file-name)))))

(defun rspec-specize-file-name (a-file-name)
  "Returns a-file-name but converted in to a spec file name"
  (concat
   (file-name-directory a-file-name)
   (replace-regexp-in-string "\\(\\.rb\\)?$" "_spec.rb" (file-name-nondirectory a-file-name))))

(defun rspec-targetize-file-name (a-file-name)
  "Returns a-file-name but converted into a non-spec file name"
     (concat (file-name-directory a-file-name)
             (rspec-file-name-with-default-extension 
              (replace-regexp-in-string "_spec\\.rb" "" (file-name-nondirectory a-file-name)))))
  
(defun rspec-file-name-with-default-extension (a-file-name)
  "Adds .rb file extension to a-file-name if it does not already have an extension"
  (if (file-name-extension a-file-name)
      a-file-name ;; file has a extension already so do nothing
    (concat a-file-name ".rb")))
        
(defun rspec-directory-subdirectories (directory)
  "Returns list of subdirectories"
  (remove-if 
   (lambda (dir) (or (string-match-p "^\\.\\.?$" (file-name-nondirectory dir)) 
                     (not (file-directory-p dir))))
   (directory-files directory t)))

(defun rspec-parent-directory (a-directory)
  "Returns the directory of which a-directory is a child"
  (file-name-directory (directory-file-name a-directory)))

(defun rspec-root-directory-p (a-directory)
  "Returns t if a-directory is the root"
  (equal a-directory (rspec-parent-directory a-directory)))
   
(defun rspec-spec-directory (a-file)
  "Returns the nearest spec directory that could contain specs for a-file"
  (if (file-directory-p a-file)
      (or
       (first (directory-files a-file t "^spec$"))
       (if (rspec-root-directory-p a-file)
           nil
         (rspec-spec-directory (rspec-parent-directory a-file))))
    (rspec-spec-directory (rspec-parent-directory a-file))))

(defun rspec-spec-file-p (a-file-name)
  "Returns true if the specified file is a spec"
  (string-match-p "\\(_\\|-\\)spec\\.rb$" a-file-name))

(defun rspec-buffer-is-spec-p ()
  "Returns true if the current buffer is a spec"
  (and (buffer-file-name)
       (rspec-spec-file-p (buffer-file-name))))





;; Makes sure that rSpec buffers are given the rspec minor mode by default
(add-hook 'ruby-mode-hook
          (lambda ()
            (when (rspec-buffer-is-spec-p)
              (rspec-mode))))

;; Add verify related spec keybinding to ruby ruby modes
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c ,") 'rspec-verify)
            (local-set-key (kbd "C-c so") 'rspec-toggle-spec-and-target)))

;; This hook makes any abbreviation that are defined in
;; rspec-mode-abbrev-table available in rSpec buffers
(add-hook 'rspec-mode-hook
          (lambda ()
            (merge-abbrev-tables rspec-mode-abbrev-table
                                 local-abbrev-table)))

;; Setup better rspec output output
(require 'mode-compile)
(add-to-list 'compilation-error-regexp-alist '("\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 2 3))
(add-to-list 'mode-compile-modes-alist '(rspec-mode . (respec-compile kill-compilation)))



(provide 'rspec-mode)