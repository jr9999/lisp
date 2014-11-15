(package-initialize)
(desktop-save-mode 1)
(require 'color-theme)
(color-theme-initialize)
(color-theme-clarity)

;(require 'layout-restore)
;(layout-restore)

(desktop-save-mode 1)
(slime)
(global-linum-mode)
(lisp-mode)
(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--core" "sbcl.core-for-slime"))))


;; layout definition
(defun my-startup-layout ()
 (interactive)
 (delete-other-windows)
 (split-window-horizontally) ;; -> |
 (next-multiframe-window)
 (find-file "~/.emacs.d/init.el")
 (split-window-vertically) ;;  -> --
 (next-multiframe-window)
 (find-file "~/.emacs.d/init_settings.el")
 (next-multiframe-window)
 (dired "~")
)

;; execute the layout
(my-startup-layout )
