;;; elegant-agenda-mode --- Towards a more elegant agenda -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>
;; URL: https://github.com/justinbarclay/elegant-agenda-mode
;; Version: 0.1.0-alpha
;; Keywords: agenda theme

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is an alpha version of the concept posted by Nicolas Rougier
;; https://old.reddit.com/r/emacs/comments/i1wfnc/one_day_in_one_frame_mockup/

;;; Code:
(defvar elegant-agenda-transforms nil)

(defvar elegant-agenda-face-remappings (let ((face-height (face-attribute 'default :height)))
                                         (list
                                          (list 'default (list :family "Yanone Kaffeesatz Light"
                                                               :height (ceiling (* face-height 1.5)) :weight 'thin))
                                          (list 'header-line (list :family "Yanone Kaffeesatz Light"
                                                                   :height (* face-height 2) :weight 'thin
                                                                   :underline nil  :overline nil :box nil))
                                          (list 'bold (list :height (ceiling (* face-height 1.1)) :weight 'light))
                                          '(italic (:foreground "orange"))
                                          '(org-link (:foreground "white"))))
  "A list of faces and the associated specs that will be remapped when elegant-agenda-mode is enabled")

(defun elegant-agenda--enable ()
  "Modifies the local buffer's state to use elegant stylings"
  (let ((width (window-width))
        (title "—  T O D A Y  "))
    (setq-local line-spacing 8)
    (setq-local org-agenda-use-time-grid nil)
    (setq-local org-agenda-block-separator "  ")
    (display-line-numbers-mode 0)
    (setq elegant-agenda-transforms
          (mapcar (lambda (face-&-spec)
                    (face-remap-add-relative (car face-&-spec) (cadr face-&-spec)))
                  elegant-agenda-face-remappings))
    (setq-local mode-line-format nil)
    (setq-local header-line-format
                (format "%s%s" title (make-string (- width (length title)) ?— t)))))

(defun elegant-agenda--disable ()
  "Resets the buffer's settings back to default"
  (setq-local line-spacing (default-value 'line-spacing))
  (setq-local org-agenda-use-time-grid (default-value 'line-spacing))
  (setq-local org-agenda-block-separator (default-value 'org-agenda-block-separator))

  (mapc #'face-remap-remove-relative
        elegant-agenda-transforms)
  (setq-local elegant-agenda-transforms nil)

  (setq-local mode-line-format (default-value 'mode-line-format))
  (setq-local header-line-format nil))

;;;###autoload
(define-minor-mode elegant-agenda-mode
  "Provides a more elegant view into your agenda"
  :init-value nil :lighter " elegant-agenda" :keymap nil
  (if elegant-agenda-mode
      (elegant-agenda--enable)
    (elegant-agenda--disable))
  (force-window-update (current-buffer)))
