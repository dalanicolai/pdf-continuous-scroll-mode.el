;;; pdf-continuous-scroll-mode.el --- Continuous scroll minor mode for pdf-tools  -*- lexical-binding: t; -*-
;; Copyright (C) 2020  Daniel Laurens Nicolai

;; Author: Daniel Laurens Nicolai <dalanicolai@gmail.com>
;; Version: 0
;; Keywords: pdf-tools,
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/dalanicolai/pdf-continuous-scroll-mode.el


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:

;;; Code:
(eval-when-compile
  (require 'pdf-view))
(require 'pdf-annot)

(defvar pdf-cscroll-mode-line-format)
(defvar pdf-cscroll-mode-line-original-face)

(defcustom pdf-continuous-step-size 4
  "Step size in lines (integer) for continuous scrolling"
  :group 'pdf-continuous-scroll
  :type 'integer)

(defcustom pdf-cs-reverse-scrolling nil
  "Reverse default scrolling direction"
  :group 'pdf-continuous-scroll
  :type 'boolean)

(defcustom pdf-cs-custom-min-height nil
  "Reverse default scrolling direction"
  :group 'pdf-continuous-scroll
  :type 'number)

(defun pdf-cscroll-window-dual-p ()
  "Return t if current scroll window status is dual, else nil."
    (or (equal 'upper (alist-get 'pdf-scroll-window-status (window-parameters)))
        (equal 'lower (alist-get 'pdf-scroll-window-status (window-parameters)))))

(defun pdf-cscroll-close-window-when-dual ()
  (when (pdf-cscroll-window-dual-p)
    (let ((window-status (alist-get 'pdf-scroll-window-status (window-parameters))))
      (save-excursion
        (if (equal window-status 'upper)
            (windmove-down)
          (windmove-up))
        (delete-window)
        (set-window-parameter nil 'pdf-scroll-window-status 'single)))))

(defun pdf-continuous-scroll-forward-line (&optional arg)
  "Scroll upward by ARG lines if possible, else go to the next page.
This function is an adapted version of
`pdf-view-next-line-or-next-page'. Although the ARG is kept here,
this function generally works best without ARG is 1. To increase
the step size for scrolling use the ARG in
`pdf-continuous-scroll-forward'"
  (if pdf-continuous-scroll-mode
         (let ((current-file buffer-file-name)
               (hscroll (window-hscroll))
               (cur-page (pdf-view-current-page)))
	         (print (format
                   "%s: window-total-height %s, frame-height %s\n
next line: vscroll value, second next line: output value (image-next-line)"
                   (alist-get 'pdf-scroll-window-status (window-parameters))
                   (window-total-height)
                   (frame-height))
                  (get-buffer-create "*pdf-scroll-log*"))
           (if (not (equal (alist-get 'pdf-scroll-window-status (window-parameters)) 'lower))
               (when (= (print
                         (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)
                         (get-buffer-create "*pdf-scroll-log*"))
                        (print (image-next-line arg) (get-buffer-create "*pdf-scroll-log*")))
	               (cond
	                ((not (window-full-height-p))
                   (condition-case nil
                       (window-resize (get-buffer-window) -1 nil t)
                     (error (delete-window)
                            (pop-to-buffer (get-file-buffer current-file))
                            (set-window-parameter nil 'pdf-scroll-window-status 'single)))
                   (image-next-line 1))
                  (t
                   (if (= (pdf-view-current-page) (pdf-cache-number-of-pages))
                       (message "No such page: %s" (+ (pdf-view-current-page) 1))
                     (display-buffer
                      (current-buffer)
                      '((display-buffer-below-selected)
                        (inhibit-same-window . t)
                        (window-height . 1)))
                     (set-window-parameter nil 'pdf-scroll-window-status 'upper)
                     (windmove-down)
                     (set-window-parameter nil 'pdf-scroll-window-status 'lower)
                     (pdf-view-goto-page cur-page)
                     (pdf-view-next-page)
                     (when (/= cur-page (pdf-view-current-page))
                       (image-bob)
                       (image-bol 1))
                     (image-set-window-hscroll hscroll)
                     (windmove-up)
                     (image-next-line 1)))))
             (condition-case nil
                 (window-resize (get-buffer-window) +1 nil t)
               (error (windmove-up)
                      (delete-window)
                      (pop-to-buffer (get-file-buffer current-file))
                      (set-window-parameter nil 'pdf-scroll-window-status 'single)))
             (windmove-up)
             (image-next-line 1)
             (windmove-down)))
  (message "pdf-continuous-scroll-mode not activated")))

(defun pdf-continuous-scroll-forward (arg)
  (interactive "P")
  (let ((arg (or arg pdf-continuous-step-size)))
    (dotimes (_ arg) (pdf-continuous-scroll-forward-line 1))))

(defun pdf-cs-mouse-scroll-forward ()
  (interactive)
  (with-selected-window
      (or (caadr last-input-event) (selected-window))
  (if pdf-cs-reverse-scrolling
      (pdf-continuous-scroll-backward nil)
    (pdf-continuous-scroll-forward nil))))

(defun pdf-continuous-scroll-backward-line (&optional arg)
  "Scroll down by ARG lines if possible, else go to the previous page.
This function is an adapted version of
`pdf-view-previous-line-or-previous-page'. Although the ARG is
kept here, this function generally works best without ARG is 1.
To increase the step size for scrolling use the ARG in
`pdf-continuous-scroll-backward'"
  (if pdf-continuous-scroll-mode
      (let ((hscroll (window-hscroll))
            (cur-page (pdf-view-current-page))
            (window-min-height (or pdf-cs-custom-min-height
                                   window-min-height)))
        (print
         (format
          "%s: window-total-height %s, frame-height %s\n
next line: vscroll value, second next line: output value (image-previous-line)"
          (alist-get 'pdf-scroll-window-status (window-parameters))
          (window-total-height)
          (frame-height))
         (get-buffer-create "*pdf-scroll-log*"))
        (cond ((equal (alist-get 'pdf-scroll-window-status (window-parameters)) 'lower)
               (cond ((= (window-total-height) window-min-height)
                      (delete-window)
                      (set-window-parameter nil 'pdf-scroll-window-status 'single)
                      (image-next-line 1))
                     (t (condition-case nil
                            (window-resize (get-buffer-window) -1 nil t)
                          (error nil))
                        (windmove-up)
                        (image-next-line 1)
                        (windmove-down))))
              (t (when (= (print
                           (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)
                           (get-buffer-create "*pdf-scroll-log*"))
                          (print
                           (image-previous-line arg)
                           (get-buffer-create "*pdf-scroll-log*")))
                   (if (= (pdf-view-current-page) 1)
                       (message "No such page: 0")
                     (display-buffer-in-direction
                      (current-buffer)
                      (cons '(direction . above) '((window-height . 1))))
                     (set-window-parameter nil 'pdf-scroll-window-status 'lower)
                     (windmove-up)
                     (set-window-parameter nil 'pdf-scroll-window-status 'upper)
                     (pdf-view-goto-page cur-page)
                     (pdf-view-previous-page)
                     (when (/= cur-page (pdf-view-current-page))
                       (image-eob)
                       (image-bol 1))
                     (image-set-window-hscroll hscroll)
                     (window-resize (get-buffer-window) 1 nil t)))
                 (cond ((< (window-total-height) (- (frame-height) window-min-height))
                        (condition-case nil
                            (window-resize (get-buffer-window) 1 nil t)
                          (error nil)))
                       ((= (window-total-height) (- (frame-height) window-min-height))
                        (set-window-parameter nil 'pdf-scroll-window-status 'single)
                        (windmove-down)
                        (delete-window))))))
    (message "pdf-continuous-scroll-mode not activated")))

(defun pdf-continuous-scroll-backward (arg)
  (interactive "P")
  (let ((arg (or arg pdf-continuous-step-size)))
    (dotimes (_ arg) (pdf-continuous-scroll-backward-line 1))))

(defun pdf-cs-mouse-scroll-backward ()
  (interactive)
  (with-selected-window
      (or (caadr last-input-event) (selected-window))
  (if pdf-cs-reverse-scrolling
      (pdf-continuous-scroll-forward nil)
    (pdf-continuous-scroll-backward nil))))

(defun pdf-continuous-next-page (arg)
  (declare (interactive-only pdf-view-previous-page))
  (interactive "p")
  (if pdf-continuous-scroll-mode
      (let ((window-status (alist-get 'pdf-scroll-window-status (window-parameters))))
        (let ((document-length (pdf-cache-number-of-pages)))
          (if (if (equal window-status 'upper)
                  (= (pdf-view-current-page) (- document-length 1))
                (= (pdf-view-current-page) document-length))
              (message "No such page: %s" (+ document-length 1))
            (cond ((equal window-status 'upper)
                   (windmove-down)
                   (with-no-warnings
                     (pdf-view-next-page arg))
                   (windmove-up)
                   (with-no-warnings
                     (pdf-view-next-page arg)))
                  ((equal window-status 'lower)
                   (windmove-up)
                   (with-no-warnings
                     (pdf-view-next-page arg))
                   (windmove-down)
                   (with-no-warnings
                     (pdf-view-next-page arg)))
                  (t (pdf-view-next-page))))))))

(defun pdf-continuous-previous-page (arg)
  (declare (interactive-only pdf-view-previous-page))
  (interactive "p")
  (if pdf-continuous-scroll-mode
      (let ((window-status (alist-get 'pdf-scroll-window-status (window-parameters))))
        (if (if (equal window-status 'lower)
                (= (pdf-view-current-page) 2)
              (= (pdf-view-current-page) 1))
            (message "No such page: 0")
          (cond ((equal window-status 'upper)
                 (windmove-down)
                 (with-no-warnings
                   (pdf-view-previous-page arg))
                 (windmove-up)
                 (with-no-warnings
                   (pdf-view-previous-page arg)))
                ((equal window-status 'lower)
                 (windmove-up)
                 (with-no-warnings
                   (pdf-view-previous-page arg))
                 (windmove-down)
                 (with-no-warnings
                   (pdf-view-previous-page arg)))
                (t (pdf-view-previous-page)))))))

(defun pdf-cscroll-view-goto-page (page &optional window)
  "Go to PAGE in PDF.

If optional parameter WINDOW, go to PAGE in all `pdf-view'
windows."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Page: "))))
  (unless (and (>= page 1)
               (<= page (pdf-cache-number-of-pages)))
    (error "No such page: %d" page))
  (pdf-cscroll-close-window-when-dual)
  (pdf-view-goto-page page window))

(defun pdf-cscroll-first-page ()
  (interactive)
  (pdf-cscroll-close-window-when-dual)
  (pdf-view-goto-page 1))

(defun pdf-cscroll-last-page ()
  (interactive)
  (pdf-cscroll-close-window-when-dual)
  (pdf-view-goto-page (pdf-cache-number-of-pages)))

(defun pdf-cscroll-kill-buffer-and-windows ()
  (interactive)
  (pdf-cscroll-close-window-when-dual)
  (kill-this-buffer))

(defun pdf-cscroll-image-forward-hscroll (&optional n)
  (interactive "p")
  (let ((window-status (alist-get 'pdf-scroll-window-status (window-parameters))))
    (cond ((equal window-status 'upper)
           (windmove-down)
           (image-forward-hscroll n)
           (windmove-up)
           (image-forward-hscroll n))
          ((equal window-status 'lower)
           (windmove-up)
           (image-forward-hscroll n)
           (windmove-down)
           (image-forward-hscroll n))
          (t (image-forward-hscroll n)))))

(defun pdf-cscroll-image-backward-hscroll (&optional n)
  (interactive "p")
  (let ((window-status (alist-get 'pdf-scroll-window-status (window-parameters))))
    (cond ((equal window-status 'upper)
           (windmove-down)
           (image-forward-hscroll (- n))
           (windmove-up)
           (image-forward-hscroll (- n)))
          ((equal window-status 'lower)
           (windmove-up)
           (image-forward-hscroll (- n))
           (windmove-down)
           (image-forward-hscroll (- n)))
          (t (image-forward-hscroll (- n))))))

(defun pdf-cscroll-toggle-mode-line ()
  (interactive)
  (if (not mode-line-format)
      (setq mode-line-format pdf-cscroll-mode-line-format)
    (setq pdf-cscroll-mode-line-format mode-line-format)
    (setq mode-line-format nil)))

(defun pdf-cscroll-toggle-narrow-mode-line ()
  (interactive)
  (if (plist-get (custom-face-attributes-get 'mode-line (selected-frame)) :height)
      (custom-set-faces
       (list 'mode-line
             (list
              (list t pdf-cscroll-mode-line-original-face))))
    (setq pdf-cscroll-mode-line-original-face
          (custom-face-attributes-get 'mode-line (selected-frame)))
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(mode-line ((t (:background "black" :height 0.1)))))
    ))

(defun pdf-cscroll-imenu ()
  (interactive)
  (pdf-cscroll-close-window-when-dual)
  (cond ((fboundp 'counsel-imenu) (counsel-imenu))
        ((fboundp 'helm-imenu) (helm-imenu))
        (t (imenu (list (imenu-choose-buffer-index))))))

(defun pdf-cscroll-annot-list-annotations ()
  (interactive)
  (pdf-cscroll-close-window-when-dual)
  (pdf-annot-list-annotations))


(setq pdf-continuous-scroll-mode-map (make-sparse-keymap))
(define-key pdf-continuous-scroll-mode-map  (kbd "C-n") #'pdf-continuous-scroll-forward)
(define-key pdf-continuous-scroll-mode-map  (kbd "<down>") #'pdf-continuous-scroll-forward)
(define-key pdf-continuous-scroll-mode-map (kbd "<wheel-down>") #'pdf-cs-mouse-scroll-forward)
(define-key pdf-continuous-scroll-mode-map  (kbd "<mouse-5>") #'pdf-cs-mouse-scroll-forward)
(define-key pdf-continuous-scroll-mode-map  (kbd "C-p") #'pdf-continuous-scroll-backward)
(define-key pdf-continuous-scroll-mode-map  (kbd "<up>") #'pdf-continuous-scroll-backward)
(define-key pdf-continuous-scroll-mode-map (kbd "<wheel-up>") #'pdf-cs-mouse-scroll-backward)
(define-key pdf-continuous-scroll-mode-map  (kbd "<mouse-4>") #'pdf-cs-mouse-scroll-backward)
(define-key pdf-continuous-scroll-mode-map  "n" #'pdf-continuous-next-page)
(define-key pdf-continuous-scroll-mode-map  "p" #'pdf-continuous-previous-page)
(define-key pdf-continuous-scroll-mode-map (kbd "<prior>") 'pdf-continuous-previous-page)
(define-key pdf-continuous-scroll-mode-map (kbd "<next>") 'pdf-continuous-next-page)
;; (define-key pdf-continuous-scroll-mode-map  (kbd "M-<") #'pdf-cscroll-view-goto-page)
(define-key pdf-continuous-scroll-mode-map  (kbd "M-g g") #'pdf-cscroll-view-goto-page)
(define-key pdf-continuous-scroll-mode-map  (kbd "M-g M-g") #'pdf-cscroll-view-goto-page)
(define-key pdf-continuous-scroll-mode-map  (kbd "M-<") #'pdf-cscroll-first-page)
(define-key pdf-continuous-scroll-mode-map  (kbd "M->") #'pdf-cscroll-last-page)
(define-key pdf-continuous-scroll-mode-map  [remap forward-char] #'pdf-cscroll-image-forward-hscroll)
(define-key pdf-continuous-scroll-mode-map  [remap right-char] #'pdf-cscroll-image-forward-hscroll)
(define-key pdf-continuous-scroll-mode-map  [remap backward-char] #'pdf-cscroll-image-backward-hscroll)
(define-key pdf-continuous-scroll-mode-map  [remap left-char] #'pdf-cscroll-image-backward-hscroll)
(define-key pdf-continuous-scroll-mode-map  "T" #'pdf-cscroll-toggle-mode-line)
(define-key pdf-continuous-scroll-mode-map  "M" #'pdf-cscroll-toggle-narrow-mode-line)
(define-key pdf-continuous-scroll-mode-map (kbd "q") '(lambda ()  (interactive) (pdf-continuous-scroll-mode -1)))
(define-key pdf-continuous-scroll-mode-map  "Q" #'pdf-cscroll-kill-buffer-and-windows)
(define-key pdf-continuous-scroll-mode-map  (kbd "C-c C-a l") #'pdf-cscroll-annot-list-annotations)

;;;###autoload
(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map  "c" #'pdf-continuous-scroll-mode))

(when (boundp 'spacemacs-version)
  (evil-define-minor-mode-key 'evilified 'pdf-continuous-scroll-mode
    "j" #'pdf-continuous-scroll-forward
    (kbd "<mouse-5>") #'pdf-cs-mouse-scroll-forward
    "k" #'pdf-continuous-scroll-backward
    (kbd "<mouse-4>") #'pdf-cs-mouse-scroll-backward
    "J" #'pdf-continuous-next-page
    "K" #'pdf-continuous-previous-page
    ;; (kbd "C-j") #'pdf-view-scroll-up-or-next-page
    ;; (kbd "C-k") #'pdf-view-scroll-down-or-previous-page
    (kbd "g t") #'pdf-cscroll-view-goto-page
    (kbd "g g") #'pdf-cscroll-first-page
    "G" #'pdf-cscroll-last-page
    "M" #'pdf-cscroll-toggle-mode-line
    "q" #'pdf-cscroll-kill-buffer-and-windows
    "l" #'pdf-cscroll-image-forward-hscroll
    "h" #'pdf-cscroll-image-backward-hscroll)
  (spacemacs/set-leader-keys-for-minor-mode
    'pdf-continuous-scroll-mode
    (kbd "a l") #'pdf-cscroll-annot-list-annotations)
  )

;;;###autoload
(define-minor-mode pdf-continuous-scroll-mode
  "Emulate continuous scroll with two synchronized buffers"
  nil
  " Continuous"
  pdf-continuous-scroll-mode-map
  (unless pdf-continuous-scroll-mode
    (pdf-cscroll-close-window-when-dual))
  (set-window-parameter nil 'pdf-scroll-window-status 'single)
  (defun pdf-outline-imenu-activate-link (&rest args)
    ;; bug #14029
    (pdf-cscroll-close-window-when-dual)
    (when (eq (nth 2 args) 'pdf-outline-imenu-activate-link)
      (setq args (cdr args)))
    (pdf-links-action-perform (nth 2 args))))


(provide 'pdf-continuous-scroll-mode)
