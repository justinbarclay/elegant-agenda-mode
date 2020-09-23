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
;; Used to revert changes when elegant-agenda-mode is disabled.
(defvar-local elegant-agenda-transforms nil "A list of faces and their associated specs.")
(defcustom elegant-agenda-font "Yanone Kaffeesatz" "The font to use in an elegant agenda buffer")
(defvar-local elegant-agenda-face-remappings
  (let ((face-height (face-attribute 'default :height)))
    (list
     (list 'default (list :family elegant-agenda-font
                          :height (ceiling (* face-height 1.5)) :weight 'thin))
     (list 'header-line (list :family elegant-agenda-font
                              :height (* face-height 2) :weight 'thin
                              :underline nil  :overline nil :box nil))
     (list 'bold (list :height (ceiling (* face-height 1.1)) :weight 'light))
     '(italic (:foreground "orange"))
     '(org-link (:foreground "white"))))
  "A list of faces and the associated specs that will be remapped
  when elegant-agenda-mode is enabled")

(defun elegant-agenda--get-title ()
  "Set an applicable title in the agenda buffer.

The title is the name of the custom command used to generate the
current view. No title will be displayed if the view was
generated from a built in command."
  (when-let ((title (when (and org-agenda-redo-command
                               (cadr org-agenda-redo-command))
                      (format "—  %s"
                              (mapconcat
                               'identity
                               (split-string-and-unquote (cadr org-agenda-redo-command) "")
                               " "))))
             (width (window-width)))
    (setq-local header-line-format
                (format "%s%s" title (make-string (- width (length title)) ?— t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle tag alignment
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proportional fonts cause an issue when tags are involved
;; https://lists.gnu.org/archive/html/emacs-orgmode/2020-05/msg00680.html
(defun elegant-agenda--string-display-pixel-width (string &optional mode)
  "Calculate pixel width of STRING.
  Optional MODE specifies major mode used for display."
  (with-temp-buffer
    (with-silent-modifications
      (setf (buffer-string) string))
    (when (fboundp mode)
      (funcall mode)
      (font-lock-fontify-buffer))
    (if (not (get-buffer-window (current-buffer)))
        (save-window-excursion
          ;; Avoid errors if the selected window is a dedicated one,
          ;; and they just want to insert a document into it.
          (set-window-dedicated-p nil nil)
          (set-window-buffer nil (current-buffer))
          (car (window-text-pixel-size nil (line-beginning-position) (point))))
      (car (window-text-pixel-size nil (line-beginning-position) (point))))))

(defun elegant-agenda--fix-tag-alignment ()
  "Use 'display :align-to instead of spaces in agenda."
  (goto-char (point-min))
  (setq-local word-wrap nil)
  (while (re-search-forward org-tag-group-re nil 'noerror)
    (put-text-property (match-beginning 0) (match-beginning 1) 'display
                       `(space . (:align-to (- right (,(string-display-pixel-width (match-string
                                                                                    1)))))))))
(defun elegant-agenda--finalize-view ()
  "Function to be called after org-agenda-finalize."
  (elegant-agenda--fix-tag-alignment)
  (elegant-agenda--get-title))

(defun elegant-agenda--enable ()
  "Set-up the current buffer to be more elegant."
  (setq-local line-spacing 8)
  (setq-local org-agenda-use-time-grid nil)
  (setq-local org-agenda-block-separator "  ")
  (display-line-numbers-mode 0)
  (setq elegant-agenda-transforms
        (mapcar (lambda (face-&-spec)
                  (face-remap-add-relative (car face-&-spec) (cadr face-&-spec)))
                elegant-agenda-face-remappings))
  (setq-local mode-line-format nil)
  (add-hook 'org-agenda-finalize-hook #'elegant-agenda--finalize-view))

(defun elegant-agenda--disable ()
  "Resets the buffer's settings back to default.

For when you're tired of being elegant."
  (setq-local line-spacing (default-value 'line-spacing))
  (setq-local org-agenda-use-time-grid (default-value 'line-spacing))
  (setq-local org-agenda-block-separator (default-value 'org-agenda-block-separator))
  (mapc #'face-remap-remove-relative
        elegant-agenda-transforms)
  (remove-hook 'org-agenda-finalize-hook #'elegant-agenda--finalize-view)
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
