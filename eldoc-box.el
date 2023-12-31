;;; eldoc-box.el --- Display documentation in childframe      -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Yuan Fu

;; Version: 1.10.1

;; Author: Yuan Fu <casouri@gmail.com>
;; URL: https://github.com/casouri/eldoc-box
;; Package-Requires: ((emacs "27.1"))

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; My modifications:
;;;
;;; - delete frame in eldoc-box-quit-frame
;;; - increase left-fringe and right-fringe
;;; - remove compatibility with company and tab-bar modes
;;; - allow to set parent major mode
;;; - add eldoc-box-toggle-help-at-point
;;; - add eldoc-box-eglot-toggle-help-at-point (legacy Eglot helper)

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'pcase)
  (require 'seq))

;;;; Userland
;;;;; Variable
(defgroup eldoc-box nil
  "Display Eldoc docs in a pretty child frame."
  :prefix "eldoc-box-"
  :group 'eldoc)

(defface eldoc-box-border '((((background dark)) . (:background "white"))
                            (((background light)) . (:background "black")))
  "The border color used in childframe.")

(defface eldoc-box-body '((t . nil))
  "Body face used in documentation childframe.")

(defvar eldoc-box-frame-parameters
  '(;; make the childframe unseen when first created
    (left . -1)
    (top . -1)
    (width  . 0)
    (height  . 0)

    (no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width  . 0)
    (min-height  . 0)
    (internal-border-width . 1)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (right-fringe . 10)
    (left-fringe . 10)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    (unsplittable . t)
    (undecorated . t)
    (visibility . nil)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (cursor-type . nil)
    (inhibit-double-buffering . t)
    (drag-internal-border . t)
    (no-special-glyphs . t)
    (desktop-dont-save . t)
    (tab-bar-lines . 0))
  "Frame parameters used to create the frame.")

(defcustom eldoc-box-max-pixel-width 800
  "Maximum width of doc childframe in pixel.
Consider your machine's screen's resolution when setting this variable.
Set it to a function with no argument
if you want to dynamically change the maximum width."
  :type 'number)

(defcustom eldoc-box-max-pixel-height 700
  "Maximum height of doc childframe in pixel.
Consider your machine's screen's resolution when setting this variable.
Set it to a function with no argument
if you want to dynamically change the maximum height."
  :type 'number)

(defvar eldoc-box-position-function #'eldoc-box--default-at-point-position-function
  "Eldoc-box uses this function to set childframe's position.
This should be a function that returns a (X . Y) cons cell.
It will be passes with two arguments: WIDTH and HEIGHT of the childframe.")

(defvar eldoc-box-buffer-hook '(eldoc-box--prettify-markdown-separator
                                eldoc-box--replace-en-space
                                eldoc-box--remove-linked-images
                                eldoc-box--remove-noise-chars
                                eldoc-box--fontify-html
                                eldoc-box--condense-large-newline-gaps)
  "Hook run after buffer for doc is setup.
Run inside the new buffer. By default, it contains some Markdown
prettifiers, which see.")

(defvar eldoc-box-frame-hook nil
  "Hook run after doc frame is setup but just before it is made visible.
Each function runs inside the new frame and receives the main frame as argument.")

;;;;; Function
(defvar eldoc-box--frame nil ;; A backstage variable
  "The frame to display doc.")

(defun eldoc-box-quit-frame ()
  "Hide documentation childframe."
  (interactive)
  (when (and eldoc-box--frame (frame-live-p eldoc-box--frame))
    (delete-frame eldoc-box--frame)
    (setq eldoc-box--frame nil)))

;;;;; Help at point

(defvar eldoc-box--help-at-point-last-point 0
  "This point cache is used by clean up function.
If (point) != last point, cleanup frame.")

(defun eldoc-box--help-at-point-cleanup ()
  "Try to clean up the childframe made by eldoc-box hack."
  (if (or (eq (point) eldoc-box--help-at-point-last-point)
          ;; don't clean up when the user clicks childframe
          (eq (selected-frame) eldoc-box--frame))
      (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup)
    (eldoc-box-quit-frame)))

;;;###autoload
(defun eldoc-box-help-at-point ()
  "Display documentation of the symbol at point."
  (interactive)
  (when (boundp 'eldoc--doc-buffer)
    (let ((str (with-current-buffer eldoc--doc-buffer (buffer-string))))
      (if (or (not str) (string= "" str))
          (error "Docs not found")
        (eldoc-box--display str)))
    (setq eldoc-box--help-at-point-last-point (point))
    (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup)))

;;;; Backstage
;;;;; Variable
(defvar eldoc-box--buffer " *eldoc-box*"
  "The buffer used to display documentation.")

;;;;; Function

;; Please compiler.
(defun eldoc-box--display (str)
  "Display STR in childframe.
STR has to be a proper documentation, not empty string, not nil, etc."
  (let ((doc-buffer (get-buffer-create eldoc-box--buffer)))
    (with-current-buffer doc-buffer
      (setq mode-line-format nil)
      (setq header-line-format nil)
      ;; WORKAROUND: (issue#66) If cursor-type is ‘box’, sometimes the
      ;; cursor is still shown for some reason.
      (setq-local cursor-type t)
      (when (bound-and-true-p global-tab-line-mode)
        (setq tab-line-format nil))
      ;; without this, clicking childframe will make doc buffer the current buffer
      (buffer-face-set 'eldoc-box-body)
      (erase-buffer)
      (insert str)
      (goto-char (point-min))
      (visual-line-mode)
      (run-hook-with-args 'eldoc-box-buffer-hook))
    (eldoc-box--get-frame doc-buffer)))

(defun eldoc-box--point-position-relative-to-native-frame (&optional point window)
  "Return (X . Y) as the coordinate of POINT in WINDOW.
The coordinate is relative to the native frame.

WINDOW nil means use selected window."
  (let* ((pos (pos-visible-in-window-p point window t))
         (x (car pos))
         (en (frame-char-width))
         (y (cadr pos))
         (edges (window-edges window nil nil t)))
    ;; HACK: for unknown reasons we need to add en to x position
    (cons (+ x (car edges) en)
          (+ y (cadr edges)))))

(defun eldoc-box--default-at-point-position-function-1 (width height)
  "See `eldoc-box--default-at-point-position-function' for WIDTH & HEIGHT docs."
  (let* ((point-pos (eldoc-box--point-position-relative-to-native-frame))
         ;; calculate point coordinate relative to native frame
         ;; because childframe coordinate is relative to native frame
         (x (car point-pos))
         (y (cdr point-pos))
         (em (frame-char-height)))
    (cons (if (< (- (frame-inner-width) width) x)
              ;; space on the right of the pos is not enough
              ;; put to left
              (max 0 (- x width))
            ;; normal, just return x
            x)
          (if (< (- (frame-inner-height) height) y)
              ;; space under the pos is not enough
              ;; put above
              (max 0 (- y height))
            ;; normal, just return y + em
            (+ y em)))))

(defun eldoc-box--default-at-point-position-function (width height)
  "Set `eldoc-box-position-function' to this function.
To have childframe appear under point.  Position is calculated
base on WIDTH and HEIGHT of childframe text window."
  (let* ((pos (eldoc-box--default-at-point-position-function-1 width height))
         (x (car pos))
         (y (cdr pos)))
    (cons x y)))

(defun eldoc-box--update-childframe-geometry (frame window)
  "Update the size and the position of childframe.
FRAME is the childframe, WINDOW is the primary window."
  ;; WORKAROUND: See issue#68. If there’s some text with a display
  ;; property of (space :width text) -- which is what we apply onto
  ;; markdown separators -- ‘window-text-pixel-size’ wouldn’t return
  ;; the correct value. Instead, it returns the current window width.
  ;; So now the childram only grows in size and never shrinks. For
  ;; whatever reason, if we set the frame size very small before
  ;; calculating window’s text size, it can return the right value.
  ;; (My guess is that the function takes (space :width text) at face
  ;; value, but that can’t be the whole picture because it works fine
  ;; when I manually evaluate the function in the childframe...)
  (set-frame-size frame 1 1 t)
  (let* ((size
          (window-text-pixel-size
           window nil nil
           (if (functionp eldoc-box-max-pixel-width) (funcall eldoc-box-max-pixel-width) eldoc-box-max-pixel-width)
           (if (functionp eldoc-box-max-pixel-height) (funcall eldoc-box-max-pixel-height) eldoc-box-max-pixel-height)
           t))
         (width (car size))
         (height (cdr size))
         (width (+ width (frame-char-width frame))) ; add margin
         (frame-resize-pixelwise t)
         (pos (funcall eldoc-box-position-function width height)))
    (set-frame-size frame width height t)
    ;; move position
    (set-frame-position frame (car pos) (cdr pos))))

(defun eldoc-box--get-frame (buffer)
  "Return a childframe displaying BUFFER.
Checkout `lsp-ui-doc--make-frame', `lsp-ui-doc--move-frame'."
  (let* ((after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (parameter (append eldoc-box-frame-parameters
                            `((default-minibuffer-frame . ,(selected-frame))
                              (minibuffer . ,(minibuffer-window))
                              (left-fringe . ,(frame-char-width)))))
         window frame
         (main-frame (selected-frame)))
    (if (and eldoc-box--frame (frame-live-p eldoc-box--frame))
        (progn (setq frame eldoc-box--frame)
               (setq window (frame-selected-window frame))
               ;; in case the main frame changed
               (set-frame-parameter frame 'parent-frame main-frame))
      (setq window (display-buffer-in-child-frame
                    buffer
                    `((child-frame-parameters . ,parameter))))
      (setq frame (window-frame window)))
    ;; workaround
    ;; (set-frame-parameter frame 'left-fringe (alist-get 'left-fringe eldoc-box-frame-parameters))
    ;; (set-frame-parameter frame 'right-fringe (alist-get 'right-fringe eldoc-box-frame-parameters))

    (set-face-attribute 'fringe frame :background 'unspecified :inherit 'eldoc-box-body)
    (set-window-dedicated-p window t)
    (redirect-frame-focus frame (frame-parent frame))
    (set-face-attribute 'internal-border frame :inherit 'eldoc-box-border)
    (when (facep 'child-frame-border)
      (set-face-background 'child-frame-border
                           (face-attribute 'eldoc-box-border :background)
                           frame))
    ;; set size
    (eldoc-box--update-childframe-geometry frame window)
    (setq eldoc-box--frame frame)
    (with-selected-frame frame
      (run-hook-with-args 'eldoc-box-frame-hook main-frame))
    (make-frame-visible frame)))


;;;; Markdown compatibility

(defun eldoc-box--prettify-markdown-separator ()
  "Prettify the markdown separator in doc returned by Eglot.
Refontify the separator so they span exactly the width of the
childframe."
  (save-excursion
    (goto-char (point-min))
    (let (prop)
      (while (setq prop (text-property-search-forward 'markdown-hr))
        (add-text-properties (prop-match-beginning prop)
                             (prop-match-end prop)
                             '(display (space :width text)
                               face (:strike-through t
                                     :height 1.5)))))))

(defun eldoc-box--replace-en-space ()
  "Display the en spaces in documentation as regular spaces."
  (face-remap-set-base 'nobreak-space '(:inherit default))
  (face-remap-set-base 'markdown-line-break-face '(:inherit default)))

(defun eldoc-box--condense-large-newline-gaps ()
  "Condense exceedingly large gaps made of consecutive newlines.

These gaps are usually made of hidden \"```\" and/or consecutive
newlines. Replace those gaps with a single empty line at 0.5 line
height."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx (>= 3 (or "\n"
                          (seq bol "```" (* (syntax word)) "\n")
                          (seq (+ "<br>") "\n")
                          (seq bol (+ (or " " "\t" " ")) "\n"))))
            nil t)
      (if (or (eq (match-beginning 0) (point-min))
              (eq (match-end 0) (point-max)))
          (replace-match "")
        (replace-match "\n\n")
        (add-text-properties (1- (point)) (point)
                             '(font-lock-face (:height 0.5)
                               face (:height 0.5)))))))

(defun eldoc-box--remove-linked-images ()
  "Some documentation embed image links in the doc...remove them."
  (save-excursion
    (goto-char (point-min))
    ;; Find every Markdown image link, and remove them.
    (while (re-search-forward
            (rx "[" (seq "![" (+? anychar) "](" (+? anychar) ")") "]"
                "(" (+? anychar) ")")
            nil t)
      (replace-match ""))))

(defun eldoc-box--remove-noise-chars ()
  "Remove some noise characters like carriage return."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

(defun eldoc-box--fontify-html ()
  "Fontify HTML tags and special entities."
  (save-excursion
    ;; <h1> tags.
    (goto-char (point-min))
    (while (re-search-forward
            (rx bol
                (group "<h" digit ">")
                (group (*? anychar))
                (group "</h" digit ">")
                eol)
            nil t)
      (add-text-properties (match-beginning 2)
                           (match-end 2)
                           '(face (:weight bold)
                             font-lock-face (:weight bold)))
      (put-text-property (match-beginning 1) (match-end 1)
                         'invisible t)
      (put-text-property (match-beginning 3) (match-end 3)
                         'invisible t))
    ;; Special entities.
    (goto-char (point-min))
    (while (re-search-forward (rx (or "&lt;" "&gt;" "&nbsp;")) nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'display
                         (pcase (match-string 0)
                           ("&lt;" "<")
                           ("&gt;" ">")
                           ("&nbsp;" " "))))))

;;------------------------------------------------------------------------------
;; My modifications
;;------------------------------------------------------------------------------

(defun eldoc-box-toggle-help-at-point ()
  "Toggle documentation of the symbol at point."
  (interactive)
  (if (and eldoc-box--frame (frame-live-p eldoc-box--frame))
      (eldoc-box-quit-frame)
    (eldoc-box-help-at-point)))

;; Parent major mode

(defvar eldoc-box--parent-major-mode nil)

(defun eldoc-box-set-parent-major-mode (parent-major-mode)
  (setq eldoc-box--parent-major-mode parent-major-mode))

;; Eglot helper based on eglot-hover-eldoc-function from eglot.el

(eval-and-compile
  (when (require 'eglot nil t)
    (require 'jsonrpc)
    (declare-function eglot--TextDocumentPositionParams "eglot.el")
    (declare-function eglot--current-server-or-lose "eglot.el")
    (declare-function eglot--hover-info "eglot.el")
    (declare-function eglot--lambda "eglot.el")
    (declare-function eglot-server-capable "eglot.el")
    (declare-function eglot--when-buffer-window "eglot.el")
    (declare-function jsonrpc-async-request "jsonrpc")

    (defun eldoc-box-eglot-toggle-help-at-point ()
      "Toggle documentation of the symbol at point."
      (interactive)
      (if (and eldoc-box--frame (frame-live-p eldoc-box--frame))
          (eldoc-box-quit-frame)
        (eldoc-box-eglot-help-at-point)))

    ;; Don't eglot--error function - it causes "error in process filter"
    ;; and freezes cursor for a while
    (defun eldoc-box-eglot-help-at-point ()
      "Display documentation of the symbol at point."
      (interactive)
      (when (eglot-server-capable :hoverProvider)
        (let ((buf (current-buffer)))
          (jsonrpc-async-request
           (eglot--current-server-or-lose)
           :textDocument/hover (eglot--TextDocumentPositionParams)
           :success-fn (eglot--lambda ((Hover) contents range)
                         (eglot--when-buffer-window buf
                           (let ((info (unless (seq-empty-p contents)
                                         (eglot--hover-info contents range))))
                             (if info
                                 (eldoc-box--display info)
                               (message "No hover info here")))))
           :deferred :textDocument/hover))
        (setq eldoc-box--help-at-point-last-point (point))
        (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup)))))

(provide 'eldoc-box)

;;; eldoc-box.el ends here
