(require 'ruby-mode)

(defconst rspec-mode-abbrev-table (make-abbrev-table))

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

(defun rspec-keymap ()
  "Creates a keymap for spec files"
  (let ((rspec-mode-map (make-sparse-keymap)))
    (define-keys rspec-mode-map
      ((kbd "C-c C-s h")  'rspec-disable-spec)
      ((kbd "C-c C-s u")  'rspec-enable-spec)
      ((kbd "C-c C-s v")  'rspec-verify))
  rspec-mode-map))


(define-minor-mode rspec-mode
  "Minor mode for rSpec files"
  :lighter " rSpec"
  :keymap  (rspec-keymap))

;; Makes sure that rSpec buffers are given the rspec minor mode by default
(add-hook 'ruby-mode-hook
          (lambda ()
            (when (string-match "_spec.rb$" (buffer-file-name))
              (rspec-mode))))

;; This hook makes any abbreviation that are defined in
;; rspec-mode-abbrev-table available in rSpec buffers
(add-hook 'rspec-mode-hook
          (lambda ()
            (merge-abbrev-tables rspec-mode-abbrev-table
                                 local-abbrev-table)))


;; Compilation

(require 'mode-compile)

;; Error and wanring patterns
(add-to-list 'compilation-error-regexp-alist
             '("\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 2 3))

(defun rspec-verify()
  "Run `ruby-command' with `rspec-flags` on current-buffer (`rspec-mode')."
  (interactive)
  (compile (concat ruby-command " " (buffer-file-name) " --format specdoc --reverse")))

(add-to-list 'mode-compile-modes-alist '(rspec-mode . (respec-compile kill-compilation)))



(provide 'rspec-mode)