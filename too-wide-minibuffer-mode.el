;;; too-wide-minibuffer-mode.el --- Shrink minibuffer if the frame is too wide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Created: 2025
;; Version: 1.0
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/hron/too-wide-minibuffer-mode
;; Keywords: convenience

;;; Commentary:

;; This package automatically assign margins to minibuffer window if it's too
;; wide.

;;; Code:

(defgroup too-wide-minibuffer nil
  "Adjust minibuffer size and position if the frame is too wide."
  :link '(url-link :tag "Website" "https://github.com/hron/too-wide-minibuffer-mode")
  :link '(emacs-library-link :tag "Library Source" "too-wide-minibuffer-mode.el")
  :group 'convenience
  :group 'minibuffer
  :prefix "too-wide-minibuffer-")

(defcustom too-wide-minibuffer-max-width 160
  "The maximum allowed width for minibuffer window to display as is."
  :group 'too-wide-minibuffer
  :type 'natnum)

(defun too-wide-minibuffer--last-window ()
  "Get the last activated window before active minibuffer."
  (let ((window (minibuffer-selected-window)))
    (or (if (window-live-p window)
            window
          (next-window))
        (selected-window))))

(defun too-wide-minibuffer--adjust-minibuffer (&optional _)
  "Adjust the size/position of minibuffer window if the frame is too wide."
  (let* ((minibuffer-win (minibuffer-window))
         (win (if (minibufferp (window-buffer (selected-window)))
                  (too-wide-minibuffer--last-window)
                (selected-window)))
         (edges (window-edges win))
         (left (nth 0 edges))
         (half-frame (/ (frame-total-cols) 2))
         (half-frame (if (cl-oddp (frame-total-cols)) (1+ half-frame) half-frame)))
    (if  (and (> (frame-width) too-wide-minibuffer-max-width)
              (>= left half-frame))
        (set-window-margins minibuffer-win half-frame 0)
      (set-window-margins minibuffer-win 0 0))))

;;;###autoload
(define-minor-mode too-wide-minibuffer-mode
  "Adjust minibuffer position/size if the frame is too wide."
  :global t
  (let ((trigger-hooks '(window-state-change-hook minibuffer-setup-hook)))
    (if too-wide-minibuffer-mode
        (dolist (hook trigger-hooks)
          (add-hook hook #'too-wide-minibuffer--adjust-minibuffer))
      (dolist (hook trigger-hooks)
        (remove-hook hook #'too-wide-minibuffer--adjust-minibuffer)))))


(provide 'too-wide-minibuffer-mode)
;;; too-wide-minibuffer-mode.el ends here
