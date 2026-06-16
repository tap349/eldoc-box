;;; eldoc-box.el --- Eglot docs popup  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Yuan Fu

;; Version: 1.10.1

;; Author: Yuan Fu <casouri@gmail.com>
;; URL: https://github.com/casouri/eldoc-box
;; Package-Requires: ((emacs "29.1"))

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

;;; Code:

(require 'eglot)
(require 'subr-x)

(defgroup eldoc-box nil
  "Display Eglot documentation in a child frame."
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
  :type '(choice integer function))

(defcustom eldoc-box-max-pixel-height 700
  "Maximum height of doc childframe in pixel.
Consider your machine's screen's resolution when setting this variable.
Set it to a function with no argument
if you want to dynamically change the maximum height."
  :type '(choice integer function))

(defcustom eldoc-box-position-function
  #'eldoc-box--default-at-point-position-function
  "Eldoc-box uses this function to set childframe's position.
This should be a function that returns a (X . Y) cons cell.
It is passed WIDTH and HEIGHT of the childframe."
  :type 'function)

(defcustom eldoc-box-position-offset '(8 . 4)
  "Pixel offset added to the default popup position.
The car is the horizontal offset, the cdr is the vertical offset."
  :type '(cons integer integer))

(defcustom eldoc-box-bottom-padding 2
  "Extra bottom padding for the popup, in pixels."
  :type 'integer)

(defvar eldoc-box-buffer-hook nil
  "Hook run in the popup buffer before it is displayed.")

(defvar eldoc-box--frame nil ;; A backstage variable
  "The frame to display doc.")

(defvar eldoc-box--source nil
  "Plist describing the buffer, window, and point for current popup.")

(defun eldoc-box-quit-frame ()
  "Hide documentation childframe."
  (interactive)
  (setq eldoc-box--source nil)
  (remove-hook 'post-command-hook #'eldoc-box--cleanup-on-move)
  (when (and eldoc-box--frame (frame-live-p eldoc-box--frame))
    (delete-frame eldoc-box--frame)
    (setq eldoc-box--frame nil)))

(defun eldoc-box--cleanup-on-move ()
  "Close the popup when point moves in its source buffer."
  (let ((buffer (plist-get eldoc-box--source :buffer))
        (point (plist-get eldoc-box--source :point)))
    (unless (or (not (buffer-live-p buffer))
                (eq (selected-frame) eldoc-box--frame)
                (and (eq (current-buffer) buffer)
                     (eq (point) (marker-position point))))
      (eldoc-box-quit-frame))))

;;;###autoload
(defun eldoc-box-toggle-help-at-point ()
  "Toggle Eglot documentation for the symbol at point."
  (interactive)
  (if (and eldoc-box--frame (frame-live-p eldoc-box--frame))
      (eldoc-box-quit-frame)
    (eldoc-box-eglot-help-at-point)))

;;;###autoload
(defun eldoc-box-eglot-help-at-point ()
  "Display Eglot documentation for the symbol at point."
  (interactive)
  (unless (eglot-current-server)
    (user-error "Current buffer is not managed by Eglot"))
  (unless (eglot-server-capable :hoverProvider)
    (user-error "Current Eglot server does not provide hover documentation"))
  (let ((source (list :buffer (current-buffer)
                      :window (selected-window)
                      :point (point-marker))))
    (setq eldoc-box--source source)
    (eglot-hover-eldoc-function
     (lambda (doc &rest _ignored)
       (when (eq source eldoc-box--source)
         (if (and (stringp doc) (not (string-empty-p doc)))
             (when-let* ((window (plist-get source :window))
                         (_ (window-live-p window)))
               (with-selected-window window
                 (eldoc-box--display doc)))
           (message "No hover info here")))))))

(defvar eldoc-box--buffer " *eldoc-box*"
  "The buffer used to display documentation.")

(defun eldoc-box--display (str)
  "Display STR in childframe.
STR has to be a proper documentation, not empty string, not nil, etc."
  (let ((doc-buffer (get-buffer-create eldoc-box--buffer)))
    (with-current-buffer doc-buffer
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil)
      ;; WORKAROUND: (issue#66) If cursor-type is ‘box’, sometimes the
      ;; cursor is still shown for some reason.
      (setq-local cursor-type t)
      (when (bound-and-true-p global-tab-line-mode)
        (setq-local tab-line-format nil))
      ;; Without this, clicking the child frame makes the doc buffer current.
      (buffer-face-set 'eldoc-box-body)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert str))
      (goto-char (point-min))
      (visual-line-mode)
      (run-hooks 'eldoc-box-buffer-hook))
    (eldoc-box--get-frame doc-buffer)))

(defun eldoc-box--point-position-relative-to-native-frame
    (&optional point window)
  "Return (X . Y) as the coordinate of POINT in WINDOW.
The coordinate is relative to the native frame.

WINDOW nil means use selected window."
  (let* ((source-window (plist-get eldoc-box--source :window))
         (source-point (plist-get eldoc-box--source :point))
         (window (or window
                     (and (window-live-p source-window) source-window)
                     (selected-window)))
         (point (or point source-point (window-point window)))
         (pos (or (pos-visible-in-window-p point window t)
                  (pos-visible-in-window-p (window-point window) window t)))
         (x (car pos))
         (en (frame-char-width))
         (y (cadr pos))
         (edges (window-edges window nil nil t)))
    ;; HACK: for unknown reasons we need to add en to x position
    (cons (+ x (car edges) en)
          (+ y (cadr edges)))))

(defun eldoc-box--source-frame ()
  "Return the frame the popup should be positioned against."
  (let ((window (plist-get eldoc-box--source :window)))
    (if (window-live-p window)
        (window-frame window)
      (selected-frame))))

(defun eldoc-box--default-at-point-position-function (width height)
  "Set `eldoc-box-position-function' to this function.
To have childframe appear under point.  Position is calculated
base on WIDTH and HEIGHT of childframe text window."
  (let* ((source-frame (eldoc-box--source-frame))
         (point-pos (eldoc-box--point-position-relative-to-native-frame))
         ;; calculate point coordinate relative to native frame
         ;; because childframe coordinate is relative to native frame
         (x (car point-pos))
         (y (cdr point-pos))
         (em (frame-char-height source-frame))
         (frame-width (frame-inner-width source-frame))
         (frame-height (frame-inner-height source-frame))
         (x-offset (car eldoc-box-position-offset))
         (y-offset (cdr eldoc-box-position-offset))
         (right-x (+ x x-offset))
         (below-y (+ y em y-offset))
         (max-x (max 0 (- frame-width width)))
         (max-y (max 0 (- frame-height height)))
         (popup-x (if (<= (+ right-x width) frame-width)
                      right-x
                    (- x width x-offset)))
         (popup-y (if (<= (+ below-y height) frame-height)
                      below-y
                    (- y height y-offset))))
    (cons (min max-x (max 0 popup-x))
          (min max-y (max 0 popup-y)))))

(defun eldoc-box--update-childframe-geometry (frame window)
  "Update the size and the position of childframe.
FRAME is the childframe, WINDOW is the primary window."
  (pcase-let ((`(,text-width . ,text-height)
               (window-text-pixel-size
                window nil nil
                (if (functionp eldoc-box-max-pixel-width)
                    (funcall eldoc-box-max-pixel-width)
                  eldoc-box-max-pixel-width)
                (if (functionp eldoc-box-max-pixel-height)
                    (funcall eldoc-box-max-pixel-height)
                  eldoc-box-max-pixel-height)
                t)))
    (let* ((width (+ text-width (frame-char-width frame)))
           (height (+ text-height eldoc-box-bottom-padding))
           (frame-resize-pixelwise t)
           (pos (funcall eldoc-box-position-function width height)))
      (set-frame-size frame width height t)
      (set-frame-position frame (car pos) (cdr pos)))))

(defun eldoc-box--get-frame (buffer)
  "Return a childframe displaying BUFFER."
  (let* ((main-frame (selected-frame))
         (parameters (append eldoc-box-frame-parameters
                             `((default-minibuffer-frame . ,main-frame)
                               (minibuffer . ,(minibuffer-window)))))
         (window
          (if (and eldoc-box--frame (frame-live-p eldoc-box--frame))
              (progn
                ;; in case the main frame changed
                (set-frame-parameter eldoc-box--frame 'parent-frame main-frame)
                (frame-selected-window eldoc-box--frame))
            (display-buffer-in-child-frame
             buffer
             `((child-frame-parameters . ,parameters)))))
         (frame (window-frame window)))
    (set-face-attribute 'fringe frame
                        :background 'unspecified
                        :inherit 'eldoc-box-body)
    (set-window-dedicated-p window t)
    (redirect-frame-focus frame (frame-parent frame))
    (set-face-attribute 'internal-border frame :inherit 'eldoc-box-border)
    (when (facep 'child-frame-border)
      (set-face-attribute 'child-frame-border frame
                          :background (face-attribute 'eldoc-box-border
                                                      :background)))
    ;; set size
    (eldoc-box--update-childframe-geometry frame window)
    (setq eldoc-box--frame frame)
    (add-hook 'post-command-hook #'eldoc-box--cleanup-on-move)
    (make-frame-visible frame)))

(provide 'eldoc-box)

;;; eldoc-box.el ends here
