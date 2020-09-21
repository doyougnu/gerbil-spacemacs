(when (configuration-layer/layer-usedp 'gerbil)

  (defun gerbil/ping! ()
    (message "pong!"))

  (defun clear-comint-buffer ()
    (interactive)
    (with-current-buffer "*scheme*"
      (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer))))

  (defun spacemacs//gerbil-spawn-repl ()
    (interactive)
    (split-window-right)
    (shrink-window-horizontally 2)
    (let ((buf (buffer-name)))
      (other-window 1)
      (run-scheme gerbil-gxi)
      ;; (switch-to-buffer-other-window "*scheme*" nil)
      ;; (switch-to-buffer buf)
      ))

  (defun spacemacs//gerbil-backend ()
    (if gerbil-backend
        ;; maintain the selection if it exists
        gerbil-backend
        ;; otherwise use the default gerbil mode
        `gerbil))

  (defun spacemacs-gerbil//setup-backend ()
    "Conditionally setup gerbil backend"
    (pcase gerbil-backend
      (`gerbil (spacemacs-gerbil//setup-gerbil))
      (`treadmill (spacemacs-gerbil//setup-treadmill))
      (`slime (spacemacs-gerbil//setup-slime))))

  (defun spacemacs-gerbil//setup-gerbil ()
    (gerbil-mode))

  (defun spacemacs-gerbil//setup-treadmill ()
    (treadmill-mode))

  )
