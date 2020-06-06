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
      (run-scheme "gxi")
      ;; (switch-to-buffer-other-window "*scheme*" nil)
      ;; (switch-to-buffer buf)
      )))
