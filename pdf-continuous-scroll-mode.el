;;; pdf-continuous-scroll-mode.el --- Continuous scroll for pdf-tools  -*- lexical-binding: t; -*-

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
(require 'pdf-tools)
(require 'pdf-annot)
(require 'image-mode)
(require 'svg)
(require 'cl-lib)

(defgroup book nil
  "Bookroll customizations.")

(defcustom book-scroll-fraction 32
  "The scroll step size in 1/fraction of page."
  :type 'integer)

(defcustom book-page-vertical-margin 5
  "The size of the vertical margins around a page."
  :type 'integer)

(defcustom pdf-cs-reverse-scrolling nil
  "Reverse default scrolling direction"
  :group 'pdf-continuous-scroll
  :type 'boolean)

(defvar-local book-number-of-pages 0)
(defvar-local book-contents-end-pos 0)

;; We overwrite the following image-mode function to make it also
;; reapply winprops when the overlay has the 'invisible property
(defun image-get-display-property ()
  (or (get-char-property (point-min) 'display
                         ;; There might be different images for different displays.
                         (if (eq (window-buffer) (current-buffer))
                             (selected-window)))
      (get-char-property (point-min) 'invisible
                         ;; There might be different images for different displays.
                         (if (eq (window-buffer) (current-buffer))
                             (selected-window)))))

(defun image-set-window-vscroll (vscroll)
  (setf (image-mode-window-get 'vscroll) vscroll
        (image-mode-window-get 'relative-vscroll) (/ (float vscroll)
                                                     (car (last (book-image-positions)))))
  (set-window-vscroll (selected-window) vscroll t))

(defun image-mode-reapply-winprops ()
  ;; When set-window-buffer, set hscroll and vscroll to what they were
  ;; last time the image was displayed in this window.
  (when (listp image-mode-winprops-alist)
    ;; Beware: this call to image-mode-winprops can't be optimized away,
    ;; because it not only gets the winprops data but sets it up if needed
    ;; (e.g. it's used by doc-view to display the image in a new window).
    (let* ((winprops (image-mode-winprops nil t))
           (hscroll (image-mode-window-get 'hscroll winprops))
           (vscroll (round (* (image-mode-window-get 'relative-vscroll winprops)
                              (car (last (book-image-positions)))))))
      (when (image-get-display-property) ;Only do it if we display an image!
	      (if hscroll (set-window-hscroll (selected-window) hscroll))
	      (if vscroll (set-window-vscroll (selected-window) vscroll t))))))

;; (defmacro book-current-page (&optional win)
;;   `(image-mode-window-get 'page ,win))
(defmacro book-overlays (&optional window) `(image-mode-window-get 'overlays ,window))
(defmacro book-image-sizes (&optional window) `(image-mode-window-get 'image-sizes ,window))
(defmacro book-image-positions (&optional window) `(image-mode-window-get 'image-positions ,window))
(defmacro book-currently-displayed-pages (&optional window) `(image-mode-window-get 'displayed-pages ,window))

(defun book-create-image-positions (image-sizes)
  (let ((sum 0)
        (positions (list 0)))
    (dolist (s image-sizes)
      (setq sum (+ sum (cdr s) (* 2 (or book-page-vertical-margin pdf-view-image-relief))))
      (push sum positions))
    (nreverse positions)))

(defun book-create-overlays-list (winprops)
  "Create list of overlays spread out over the buffer contents.
Pass non-nil value for include-first when the buffer text starts with a match."
  ;; first overlay starts at 1
  ;; (setq book-contents-end-pos (goto-char (point-max)))
  (goto-char book-contents-end-pos)
  (let ((eobp (eobp))
        overlays)
    (if (eobp)
        (insert " ")
      (forward-char))
    (push (make-overlay (1- (point)) (point)) overlays)
    (let ((overlays-list (dotimes (_ (1- (length (book-image-sizes))) (nreverse overlays))
                           (if eobp
                               (insert "\n ")
                             (forward-char 2))
                           (push (make-overlay (1- (point)) (point)) overlays))))
      (mapc (lambda (o) (overlay-put o 'window (car winprops))) overlays-list)
      (image-mode-window-put 'overlays overlays-list winprops)))
  (goto-char (point-min))
  (set-buffer-modified-p nil))

(defun book-create-empty-page (size)
  (pcase-let* ((`(,w . ,h) size))
    (svg-image (svg-create w h)
               :margin (cons 0 book-page-vertical-margin))))

(defun book-create-placeholders ()
  (let* ((constant-size (cl-every #'eql (book-image-sizes) (cdr (book-image-sizes))))
         (ph (when constant-size (book-create-empty-page (car (book-image-sizes))))))
    (dotimes (i (length (book-image-sizes)))
      ;; (let ((p (1+ i)));; shift by 1 to match with page numbers
      ;; (overlay-put (nth i overlays-list) 'display (or ph (book-create-empty-page (nth i (book-image-sizes))))))))
      (overlay-put (nth i (book-overlays)) 'display (or ph (book-create-empty-page (nth i (book-image-sizes))))))))

(defun book-current-page ()
  (interactive)
  (let ((i 0)
        (cur-pos (window-vscroll nil t)))
    (while (<= (nth (1+ i) (book-image-positions)) (+ cur-pos (/ (window-pixel-height) 2)))
      (setq i (1+ i)))
    (1+ i)))

(defun book-page-triplet (page)
  (pcase (pdf-info-number-of-pages)
    (1 '(1))
    (2 '(1 2))
    (_ (pcase page
         (1 '(1 2))
         ((pred (= book-number-of-pages)) (list page (- page 1)))
         (p (list (- p 1) p (+ p 1)))))))

(defun book-remove-page-image (page)
  (overlay-put (nth (1- page) (book-overlays))
               'display
               (book-create-empty-page (nth (1- page) (book-image-sizes)))))


(defun book-scroll-to-page (page)
  (interactive "n")
  ;; (book-update-page-triplet page)
  (let* ((elt (1- page)))
    (set-window-vscroll nil (+ (nth elt (book-image-positions)) (or book-page-vertical-margin pdf-view-image-relief)) t)))

(defvar pdf-continuous-suppress-introduction nil)

(define-derived-mode pdf-view-mode special-mode "PDFView"
  "Major mode in PDF buffers.

PDFView Mode is an Emacs PDF viewer.  It displays PDF files as
PNG images in Emacs buffers."
  :group 'pdf-view
  :abbrev-table nil
  :syntax-table nil
  ;; Setup a local copy for remote files.
  (when (and (or jka-compr-really-do-compress
                 (let ((file-name-handler-alist nil))
                   (not (and buffer-file-name
                             (file-readable-p buffer-file-name)))))
             (pdf-tools-pdf-buffer-p))
    (let ((tempfile (pdf-util-make-temp-file)))
      (write-region nil nil tempfile nil 'no-message)
      (setq-local pdf-view--buffer-file-name tempfile)))
  ;; Decryption needs to be done before any other function calls into
  ;; pdf-info.el (e.g. from the mode-line during redisplay during
  ;; waiting for process output).
  (pdf-view-decrypt-document)

  ;; Setup scroll functions
  (if (boundp 'mwheel-scroll-up-function) ; not --without-x build
      (setq-local mwheel-scroll-up-function
                  #'pdf-view-scroll-up-or-next-page))
  (if (boundp 'mwheel-scroll-down-function)
      (setq-local mwheel-scroll-down-function
                  #'pdf-view-scroll-down-or-previous-page))

  ;; Clearing overlays
  (add-hook 'change-major-mode-hook
            (lambda ()
              (remove-overlays (point-min) (point-max) 'pdf-view t))
            nil t)
  (remove-overlays (point-min) (point-max) 'pdf-view t) ;Just in case.

  ;; Setup other local variables.
  (setq-local mode-line-position
              '(" P" (:eval (number-to-string (pdf-view-current-page)))
                ;; Avoid errors during redisplay.
                "/" (:eval (or (ignore-errors
                                 (number-to-string (pdf-cache-number-of-pages)))
                               "???"))))
  (setq-local auto-hscroll-mode nil)
  (setq-local pdf-view--server-file-name (pdf-view-buffer-file-name))
  ;; High values of scroll-conservatively seem to trigger
  ;; some display bug in xdisp.c:try_scrolling .
  (setq-local scroll-conservatively 0)
  (setq-local cursor-type nil)
  (setq-local buffer-read-only t)
  (setq-local view-read-only nil)
  (setq-local bookmark-make-record-function
              'pdf-view-bookmark-make-record)
  (setq-local revert-buffer-function #'pdf-view-revert-buffer)
  ;; No auto-save at the moment.
  (setq-local buffer-auto-save-file-name nil)
  ;; Disable image rescaling.
  (when (boundp 'image-scaling-factor)
    (setq-local image-scaling-factor 1))
  ;; No undo at the moment.
  (unless buffer-undo-list
    (buffer-disable-undo))
  ;; Enable transient-mark-mode, so region deactivation when quitting
  ;; will work.
  (setq-local transient-mark-mode t)
  ;; In Emacs >= 24.4, `cua-copy-region' should have been advised when
  ;; loading pdf-view.el so as to make it work with
  ;; pdf-view-mode. Disable cua-mode if that is not the case.
  ;; FIXME: cua-mode is a global minor-mode, but setting cua-mode to
  ;; nil seems to do the trick.
  (when (and (bound-and-true-p cua-mode)
             (version< emacs-version "24.4"))
    (setq-local cua-mode nil))

  (setq-local book-contents-end-pos (point-max))
  (setq-local book-number-of-pages (pdf-cache-number-of-pages))

  (add-hook 'window-configuration-change-hook
            'pdf-view-redisplay-some-windows nil t)
  (add-hook 'deactivate-mark-hook 'pdf-view-deactivate-region nil t)
  (add-hook 'write-contents-functions
            'pdf-view--write-contents-function nil t)
  (add-hook 'kill-buffer-hook 'pdf-view-close-document nil t)
  (pdf-view-add-hotspot-function
   'pdf-view-text-regions-hotspots-function -9)

  ;; Keep track of display info
  (add-hook 'image-mode-new-window-functions
            'pdf-view-new-window-function nil t)
  (image-mode-setup-winprops)

  (unless pdf-continuous-suppress-introduction
    (pdf-continuous-introduce))

  ;; Issue a warning in the future about incompatible modes.
  (run-with-timer 1 nil (lambda (buffer)
                          (when (buffer-live-p buffer)
                            (pdf-view-check-incompatible-modes buffer)

                            (unless pdf-continuous-suppress-introduction
                              (switch-to-buffer "*pdf-continuous-introduction*"))))
		              (current-buffer)))

(defun pdf-continuous-toggle-message ()
  (interactive)
  (setq pdf-continuous-suppress-introduction
        (if pdf-continuous-suppress-introduction
            nil
          (message "pdf-continuous message suppressed")
          t)))

(defun pdf-continuous-introduce ()
  (with-current-buffer (get-buffer-create "*pdf-continuous-introduction*")
    (insert "NEW PDF CONTINUOUS SCROLL: INTRODUCTION            3 February 2022

Welcome to the new pdf-continuous-scroll-mode, now finally
providing continuous scroll in a single buffer. 🎉🍾

My apologies for this rude interruption, however this behavior is
only temporary (until this functionality gets merged into
pdf-tools in May or so).

There are two reasons for this interruption:

- second, to inform you about how to obtain or set indicators
  allowing you to differentiate between the pages. The default
  design uses the customizable `book-page-vertical-margin'
  variable, which sets vertical margins for the page images. If
  your Emacs theme has a different background color than your
  books page color, this will nicely indicate the page
  'transitions'. However, if the background color and the page
  color are the same, then you can set the
  `book-page-vertical-margin' to 0 and instead set
  `pdf-view-image-relief' to some non negative number to help you
  differentiate between the pages.

  Also, redisplay (e.g. splitting buffers), does not work
  flawlessly yet, but you can simply split and start scrolling,
  or use `M-x pdf-view-goto-page', in the buffers and the display
  problem will 'fix itself'. (To fix the root of the problem, I
  probably have to make `image-mode-reapply-winprops' use
  'relative' instead of `absolute' vscroll. Also, I have noticed,
  that, while in Spacemacs redisplay works almost fine, in
  vanilla Emacs the vscroll does not get restored despite
  `image-mode-reapply-winprops' getting called.)

- first, I would like to inform you that I would be very happy
  with any small donation if you can afford it (I guess most
  Emacs PDF-tools users are students). Despite the low number of
  lines of code here (of which a large part is adapted from
  pdf-view.el), creating this package has cost me a lot of
  effort. I think writing the code has only cost me about 0.001
  percent of the time, while most of the time has been invested
  in investigating pdf-tools and doc-view and their very opaque
  and non-trivial display mechanisms.

  You can find donate buttons on the
  pdf-continuous-scroll-mode.el github page
  (https://github.com/dalanicolai/pdf-continuous-scroll-mode.el).

  With the help of donations I could work on fixing bugs and on
  improving pdf-tools and image-mode documentation (which are
  really great packages, but they lack documentation. If about 50
  lines of documentation had been available, it would probably
  have saved me two/three weeks of work).

  Besides that, I have also written an alternative pdf
  server (https://github.com/vedang/pdf-tools/pull/61) using
  python with the excellent pymupdf
  package (https://pymupdf.readthedocs.io/en/latest/). Again in
  that case, writing the code was only a tiny fraction of the
  total work. This alternative server provides new kinds of
  annotation functionality like line, arrow and free text
  annotations. Furthermore, it offers the possibility to send
  python code directly to the server, so that it is possible to
  use the full features provided by pymupdf.

  The combined work has cost me almost two months of almost full
  time investigating, debugging, iterating.

  To save you a long interesting story, I just mention that
  currently I am dependent on others for paying my rent and my
  food. Otherwise, I would have been even more happy by
  sharing/donating this package without asking for any donations.

  I have created a few more packages that can be found and are
  described at my github profile
  page (https://github.com/dalanicolai).

You can toggle off this message by doing `M-x
pdf-continuous-toggle-message' or setting
`pdf-continuous-suppress-introduction' to non-nil in your
dotfile. Or you can simply close this buffer and start reading.

Thank you.
Daniel

Happy scrolling!"
)
    (goto-char (point-min))))


(defun pdf-view-goto-page (page &optional window)
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
  (unless window
    (setq window
          (if (pdf-util-pdf-window-p)
              (selected-window)
            t)))
  (save-selected-window
    ;; Select the window for the hooks below.
    (when (window-live-p window)
      (select-window window 'norecord))
    (let ((changing-p
           (not (eq page (pdf-view-current-page window)))))
      (when changing-p
        (run-hooks 'pdf-view-before-change-page-hook)
        (setf (pdf-view-current-page window) page)
        (run-hooks 'pdf-view-change-page-hook))
      (when (window-live-p window)
        (pdf-view-redisplay window))
      (when changing-p
        (pdf-view-deactivate-region)
        (force-mode-line-update)
        (run-hooks 'pdf-view-after-change-page-hook))))
  (image-set-window-vscroll (+ (nth (1- page) (book-image-positions))
                               (or book-page-vertical-margin pdf-view-image-relief)))
  nil)

(defun pdf-continuous-scroll-forward (&optional pixels)
  ;; (defun pdf-view-next-line-or-next-page ()
  (interactive)
  ;; because pages could have different heights, we calculate the step size on each scroll
  ;; TODO define constant scroll size if doc has single page height
  (let* ((scroll-step-size (/ (cdr (pdf-view-image-size)) book-scroll-fraction))
         (page-end (nth (pdf-view-current-page) (book-image-positions)))
         (vscroll (window-vscroll nil t))
         (new-vscroll (image-set-window-vscroll (if (< vscroll (- (car (last (book-image-positions)))
                                                                  (window-pixel-height)))
                                                    (+ vscroll (or pixels scroll-step-size))
                                                  (message "End of book")
                                                  vscroll))))
    (when (> (+ new-vscroll (/ (window-pixel-height) 2)) page-end)
      (let ((old-page (pdf-view-current-page))
            (new-page (alist-get 'page (cl-incf (pdf-view-current-page)))))
        (when (> old-page 1)
          (book-remove-page-image (1- old-page))
          (setf (book-currently-displayed-pages) (delete (1- old-page) (book-currently-displayed-pages))))
        (when (< new-page (pdf-info-number-of-pages))
          (pdf-view-display-triplet new-page)))))
                                    ;; :width doc-view-image-width
                                    ;; :pointer 'arrow
                                    ;; :margin (cons 0 book-page-vertical-margin))))))
  (sit-for 0))

(defun pdf-continuous-scroll-backward (&optional pixels)
  ;; (defun pdf-view-next-line-or-next-page ()
  (interactive)
  ;; because pages could have different heights, we calculate the step size on each scroll
  ;; TODO define constant scroll size if doc has single page height
  (let* ((scroll-step-size (/ (cdr (pdf-view-image-size)) book-scroll-fraction))
         (page-beg (nth (1- (pdf-view-current-page)) (book-image-positions)))
         (new-vscroll (image-set-window-vscroll (- (window-vscroll nil t) (or pixels scroll-step-size)))))
    (when (< (+ new-vscroll (/ (window-pixel-height) 2)) page-beg)
      (let ((old-page (pdf-view-current-page))
            (new-page (alist-get 'page (cl-decf (pdf-view-current-page)))))
        (when (< old-page (pdf-info-number-of-pages))
          (book-remove-page-image (1+ old-page))
          (setf (book-currently-displayed-pages) (delete (1+ old-page) (book-currently-displayed-pages))))
        (when (> new-page 1)
          (pdf-view-display-triplet new-page)))))
                                 ;; :width doc-view-image-width
                                 ;; :pointer 'arrow
                                 ;; :margin (cons 0 book-page-vertical-margin))))))
  (sit-for 0))

(defun pdf-cs-mouse-scroll-forward ()
  (interactive)
  (with-selected-window
      (or (caadr last-input-event) (selected-window))
    (if pdf-cs-reverse-scrolling
        (pdf-continuous-scroll-backward nil)
      (pdf-continuous-scroll-forward nil))))

(defun pdf-cs-mouse-scroll-backward ()
  (interactive)
  (with-selected-window
      (or (caadr last-input-event) (selected-window))
    (if pdf-cs-reverse-scrolling
        (pdf-continuous-scroll-forward nil)
      (pdf-continuous-scroll-backward nil))))

(defun pdf-continuous-next-page ()
  (interactive)
  (pdf-continuous-scroll-forward (+ (cdr (nth (1- (book-current-page)) (book-image-sizes)))
                                    (* 2 (or book-page-vertical-margin pdf-view-image-relief)))))

(defun pdf-continuous-previous-page ()
  (interactive)
  (pdf-continuous-scroll-backward (+ (cdr (nth (1- (book-current-page)) (book-image-sizes)))
                                    (* 2 (or book-page-vertical-margin pdf-view-image-relief)))))

(defun pdf-cscroll-first-page ()
  (interactive)
  (pdf-view-goto-page 1))

(defun pdf-cscroll-last-page ()
  (interactive)
  (pdf-view-goto-page (pdf-cache-number-of-pages)))

(defun pdf-view-create-page (page &optional window)
  "Create an image of PAGE for display on WINDOW."
  (let* ((size (pdf-view-desired-image-size page window))
         (data (pdf-cache-renderpage
                page (car size)
                (if (not pdf-view-use-scaling)
                    (car size)
                  (* 2 (car size)))))
         (hotspots (pdf-view-apply-hotspot-functions
                    window page size)))
    (pdf-view-create-image data
      :width (car size)
      :margin (cons 0 book-page-vertical-margin)
      :map hotspots
      :pointer 'arrow)))

(defun pdf-view-image-size (&optional displayed-p window)
  ;; TODO: add WINDOW to docstring.
  "Return the size in pixel of the current image.

If DISPLAYED-P is non-nil, return the size of the displayed
image.  These values may be different, if slicing is used."
  ;; (if displayed-p
  ;;     (with-selected-window (or window (selected-window))
  ;;       (image-display-size
  ;;        (image-get-display-property) t))
  (image-size (pdf-view-current-image window) t))

(defun pdf-view-display-page (page &optional window)
  "Display page PAGE in WINDOW."
  (with-selected-window (or window (selected-window))
    (when (book-overlays)
      (mapcar #'delete-overlay
              (book-overlays))
      (setf (book-overlays) nil))

    (let* ((image-sizes (let (s)
                          (dotimes (i (pdf-info-number-of-pages) (nreverse s))
                            (push (pdf-view-desired-image-size (1+ i)) s))))
           (image-positions (book-create-image-positions image-sizes)))
      (image-mode-window-put 'image-sizes image-sizes)
      (image-mode-window-put 'image-positions image-positions))

    (let ((inhibit-read-only t))
      (book-create-overlays-list (image-mode-winprops window))
      (book-create-placeholders)))

  (setf (pdf-view-window-needs-redisplay window) nil)
  (setf (pdf-view-current-page window) page)

  (pdf-view-display-triplet page window))

(defun pdf-view-display-triplet (page &optional window inhibit-slice-p)
  ;; TODO: write documentation!
  (let ((ol (pdf-view-current-overlay window))
        (display-pages (book-page-triplet page)))
    (when (window-live-p (overlay-get ol 'window))
      (dolist (p (book-currently-displayed-pages window))
        (unless (member p display-pages)
          (book-remove-page-image p)))
      (dolist (p display-pages)
        (let* ((image (pdf-view-create-page p window))
               (size (image-size image t))
               (slice (if (not inhibit-slice-p)
                          (pdf-view-current-slice window)))
               (displayed-width (floor
                                 (if slice
                                     (* (nth 2 slice)
                                        (car (image-size image)))
                                   (car (image-size image))))))
          (when (= p page)
            (setf (pdf-view-current-image window) image))
          ;; In case the window is wider than the image, center the image
          ;; horizontally.
          (overlay-put (nth (1- p) (book-overlays window)) 'before-string
                       (when (> (window-width window)
                                displayed-width)
                         (propertize " " 'display
                                     `(space :align-to
                                             ,(/ (- (window-width window)
                                                    displayed-width) 2)))))
          ;; (message "%s %s" p window)
          (overlay-put (nth (1- p) (book-overlays window)) 'display
                       (if slice
                           (list (cons 'slice
                                       (pdf-util-scale slice size 'round))
                                 image)
                         image))
          (push p (book-currently-displayed-pages window))))
      (let* ((win (overlay-get ol 'window))
             (hscroll (image-mode-window-get 'hscroll win))
             (vscroll (if-let (vs (image-mode-window-get 'relative-vscroll (image-mode-winprops)))
                          (round (* vs
                                    (car (last (book-image-positions window)))))
                        (image-mode-window-get 'vscroll win))))
        ;; Reset scroll settings, in case they were changed.
        (if hscroll (set-window-hscroll win hscroll))
        (if vscroll (set-window-vscroll
                     win vscroll pdf-view-have-image-mode-pixel-vscroll)))
      (setq currently-displayed-pages display-pages))))

(defun pdf-view-display-image (image &optional window inhibit-slice-p)
  ;; TODO: write documentation!
  (let ((ol (pdf-view-current-overlay window)))
    (when (window-live-p (overlay-get ol 'window))
      (let* ((size (image-size image t))
             (slice (if (not inhibit-slice-p)
                        (pdf-view-current-slice window)))
             (displayed-width (floor
                               (if slice
                                   (* (nth 2 slice)
                                      (car (image-size image)))
                                 (car (image-size image)))))
             (p (pdf-view-current-page)))
        (setf (pdf-view-current-image window) image)
        ;; (move-overlay ol (point-min) (point-max))
        ;; In case the window is wider than the image, center the image
        ;; horizontally.
        (overlay-put (nth (1- p) (book-overlays)) 'before-string
                     (when (> (window-width window)
                              displayed-width)
                       (propertize " " 'display
                                   `(space :align-to
                                           ,(/ (- (window-width window)
                                                  displayed-width) 2)))))
        (overlay-put (nth (1- p) (book-overlays)) 'display
                     (if slice
                         (list (cons 'slice
                                     (pdf-util-scale slice size 'round))
                               image)
                       image))
        (let* ((win (overlay-get ol 'window))
               (hscroll (image-mode-window-get 'hscroll win))
               (vscroll (image-mode-window-get 'vscroll win)))
          ;; Reset scroll settings, in case they were changed.
          (if hscroll (set-window-hscroll win hscroll))
          (if vscroll (set-window-vscroll
                       win vscroll pdf-view-have-image-mode-pixel-vscroll)))))))

(defun pdf-view-redisplay (&optional window)
  "Redisplay page in WINDOW.

If WINDOW is t, redisplay pages in all windows."
  (unless pdf-view-inhibit-redisplay
    (if (not (eq t window))
        (pdf-view-display-page
         (pdf-view-current-page window)
         window)
      (dolist (win (get-buffer-window-list nil nil t))
        (pdf-view-display-page
         (pdf-view-current-page win)
         win))
      (when (consp image-mode-winprops-alist)
        (dolist (window (mapcar #'car image-mode-winprops-alist))
          (unless (or (not (window-live-p window))
                      (eq (current-buffer)
                          (window-buffer window)))
            (setf (pdf-view-window-needs-redisplay window) t)))))
    (force-mode-line-update)))

(defun pdf-view-redisplay-some-windows ()
  (pdf-view-maybe-redisplay-resized-windows)
  (dolist (window (get-buffer-window-list nil nil t))
    ;; (when (pdf-view-window-needs-redisplay window)
    (pdf-view-redisplay window)))

(defun pdf-view-new-window-function (winprops)
  ;; TODO: write documentation!
  ;; (message "New window %s for buf %s" (car winprops) (current-buffer))
  (cl-assert (or (eq t (car winprops))
                 (eq (window-buffer (car winprops)) (current-buffer))))
  (let ((ol (image-mode-window-get 'overlay winprops)))
    (if ol
        (progn
          (setq ol (copy-overlay ol))
          ;; `ol' might actually be dead.
          (move-overlay ol (point-min) book-contents-end-pos))
      (setq ol (make-overlay (point-min) book-contents-end-pos nil t))
      (overlay-put ol 'pdf-view t))
    (overlay-put ol 'window (car winprops))
    (unless (windowp (car winprops))
      ;; It's a pseudo entry.  Let's make sure it's not displayed (the
      ;; `window' property is only effective if its value is a window).
      (cl-assert (eq t (car winprops)))
      (delete-overlay ol))
    (image-mode-window-put 'overlay ol winprops)
    ;; Clean up some overlays.
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (and (windowp (overlay-get ov 'window))
                 (not (window-live-p (overlay-get ov 'window))))
        (delete-overlay ov)))
    (when (windowp (car winprops))
      (overlay-put ol 'invisible t)
      (let* ((image-sizes (let (s)
                            (dotimes (i (pdf-info-number-of-pages) (nreverse s))
                              (push (pdf-view-desired-image-size (1+ i)) s))))
             (image-positions (book-create-image-positions image-sizes)))
        (image-mode-window-put 'image-sizes image-sizes winprops)
        (image-mode-window-put 'image-positions image-positions winprops))
      (let ((inhibit-read-only t))
        (book-create-overlays-list winprops)
        (book-create-placeholders))
      ;; We're not displaying an image yet, so let's do so.  This
      ;; happens when the buffer is displayed for the first time.
      ;; (when (null (print (image-mode-window-get 'image winprops)))
        (with-selected-window (car winprops)
          (pdf-view-goto-page
           (or (image-mode-window-get 'page t) 1))))))

(defun pdf-view-mouse-set-region (event &optional allow-extend-p
                                        rectangle-p)
  "Select a region of text using the mouse with mouse event EVENT.

Allow for stacking of regions, if ALLOW-EXTEND-P is non-nil.

Create a rectangular region, if RECTANGLE-P is non-nil.

Stores the region in `pdf-view-active-region'."
  (interactive "@e")
  (setq pdf-view--have-rectangle-region rectangle-p)
  (unless (and (eventp event)
               (mouse-event-p event))
    (signal 'wrong-type-argument (list 'mouse-event-p event)))
  (unless (and allow-extend-p
               (or (null (get this-command 'pdf-view-region-window))
                   (equal (get this-command 'pdf-view-region-window)
                          (selected-window))))
    (pdf-view-deactivate-region))
  (put this-command 'pdf-view-region-window
       (selected-window))
  (let* ((window (selected-window))
         (pos (event-start event))
         (begin-inside-image-p t)
         (begin (if (posn-image pos)
                    (posn-object-x-y pos)
                  (setq begin-inside-image-p nil)
                  (posn-x-y pos)))
         (abs-begin (posn-x-y pos))
         pdf-view-continuous
         region)
    (when (pdf-util-track-mouse-dragging (event 0.05)
            (message "1 %s" (window-vscroll nil t))
            (let* ((pos (event-start event))
                   (end (posn-object-x-y pos))
                   (end-inside-image-p
                    (and (eq window (posn-window pos))
                         (posn-image pos))))
              (when (or end-inside-image-p
                        begin-inside-image-p)
                (cond
                 ((and end-inside-image-p
                       (not begin-inside-image-p))
                  ;; Started selection outside the image, setup begin.
                  (let* ((xy (posn-x-y pos))
                         (dxy (cons (- (car xy) (car begin))
                                    (- (cdr xy) (cdr begin))))
                         (size (pdf-view-image-size t)))
                    (setq begin (cons (max 0 (min (car size)
                                                  (- (car end) (car dxy))))
                                      (max 0 (min (cdr size)
                                                  (- (cdr end) (cdr dxy)))))
                          ;; Store absolute position for later.
                          abs-begin (cons (- (car xy)
                                             (- (car end)
                                                (car begin)))
                                          (- (cdr xy)
                                             (- (cdr end)
                                                (cdr begin))))
                          begin-inside-image-p t)))
                 ((and begin-inside-image-p
                       (not end-inside-image-p))
                  ;; Moved outside the image, setup end.
                  (let* ((xy (posn-x-y pos))
                         (dxy (cons (- (car xy) (car abs-begin))
                                    (- (cdr xy) (cdr abs-begin))))
                         (size (pdf-view-image-size t)))
                    (setq end (cons (max 0 (min (car size)
                                                (+ (car begin) (car dxy))))
                                    (max 0 (min (cdr size)
                                                (+ (cdr begin) (cdr dxy)))))))))
                (let ((iregion (if rectangle-p
                                   (list (min (car begin) (car end))
                                         (min (cdr begin) (cdr end))
                                         (max (car begin) (car end))
                                         (max (cdr begin) (cdr end)))
                                 (list (car begin) (cdr begin)
                                       (car end) (cdr end)))))
                  (setq region
                        (pdf-util-scale-pixel-to-relative iregion))
                  (message "2 %s" (window-vscroll nil t))
                  (pdf-view-display-region
                   (cons region pdf-view-active-region)
                   rectangle-p)
                  ;; the following somehow messes up activating regions
                  ;; (pdf-util-scroll-to-edges iregion)
                  ))))
      (setq pdf-view-active-region
            (append pdf-view-active-region
                    (list region)))
      (pdf-view--push-mark))))


;;; Fix jump to link (from outline)

(defun pdf-links-action-perform (link)
  "Follow LINK, depending on its type.

This may turn to another page, switch to another PDF buffer or
invoke `pdf-links-browse-uri-function'.

Interactively, link is read via `pdf-links-read-link-action'.
This function displays characters around the links in the current
page and starts reading characters (ignoring case).  After a
sufficient number of characters have been read, the corresponding
link's link is invoked.  Additionally, SPC may be used to
scroll the current page."
  (interactive
   (list (or (pdf-links-read-link-action "Activate link (SPC scrolls): ")
             (error "No link selected"))))
  (let-alist link
    (cl-case .type
      ((goto-dest goto-remote)
       (let ((window (selected-window)))
         (cl-case .type
           (goto-dest
            (unless (> .page 0)
              (error "Link points to nowhere")))
           (goto-remote
            (unless (and .filename (file-exists-p .filename))
              (error "Link points to nonexistent file %s" .filename))
            (setq window (display-buffer
                          (or (find-buffer-visiting .filename)
                              (find-file-noselect .filename))))))
         (with-selected-window window
           (when (derived-mode-p 'pdf-view-mode)
             (when (> .page 0)
               (pdf-view-goto-page .page))
             (when .top
               ;; Showing the tooltip delays displaying the page for
               ;; some reason (sit-for/redisplay don't help), do it
               ;; later.

               ;; TODO fix lambda/pdf-util-tooltip-arrow for compatibility with
               ;; continuous scroll
               ;; (run-with-idle-timer 0.001 nil
               ;;                      (lambda ()
               ;;                        (when (window-live-p window)
               ;;                          (with-selected-window window
               ;;                            (when (derived-mode-p 'pdf-view-mode)
               ;;                              (pdf-util-tooltip-arrow .top))))))
               )
             ))))
      (uri
       (funcall pdf-links-browse-uri-function .uri))
      (t
       (error "Unrecognized link type: %s" .type)))
    nil))


;;; Fix occur (TODO fix isearch and remove this function)

(defun pdf-occur-goto-occurrence (&optional no-select-window-p)
  "Go to the occurrence at point.

If EVENT is nil, use occurrence at current line.  Select the
PDF's window, unless NO-SELECT-WINDOW-P is non-nil.

FIXME: EVENT not used at the moment."
  (interactive)
  (let ((item (tabulated-list-get-id)))
    (when item
      (let* ((doc (plist-get item :document))
             (page (plist-get item :page))
             (match (plist-get item :match-edges))
             (buffer (if (bufferp doc)
                         doc
                       (or (find-buffer-visiting doc)
                           (find-file-noselect doc))))
             window)
        (if no-select-window-p
            (setq window (display-buffer buffer))
          (pop-to-buffer buffer)
          (setq window (selected-window)))
        (with-selected-window window
          (when page
            (pdf-view-goto-page page))
          ;; Abuse isearch.
          ;; (when match
          ;;   (let ((pixel-match
          ;;          (pdf-util-scale-relative-to-pixel match))
          ;;         (pdf-isearch-batch-mode t))
          ;;     (pdf-isearch-hl-matches pixel-match nil t)
          ;;     (pdf-isearch-focus-match-batch pixel-match)))
          )))))


(define-key pdf-view-mode-map  (kbd "C-n") #'pdf-continuous-scroll-forward)
(define-key pdf-view-mode-map  (kbd "<down>") #'pdf-continuous-scroll-forward)
(define-key pdf-view-mode-map (kbd "<wheel-down>") #'pdf-cs-mouse-scroll-forward)
(define-key pdf-view-mode-map  (kbd "<mouse-5>") #'pdf-cs-mouse-scroll-forward)
(define-key pdf-view-mode-map  (kbd "C-p") #'pdf-continuous-scroll-backward)
(define-key pdf-view-mode-map  (kbd "<up>") #'pdf-continuous-scroll-backward)
(define-key pdf-view-mode-map (kbd "<wheel-up>") #'pdf-cs-mouse-scroll-backward)
(define-key pdf-view-mode-map  (kbd "<mouse-4>") #'pdf-cs-mouse-scroll-backward)
(define-key pdf-view-mode-map  "n" #'pdf-continuous-next-page)
(define-key pdf-view-mode-map  "p" #'pdf-continuous-previous-page)
(define-key pdf-view-mode-map (kbd "<prior>") 'pdf-continuous-previous-page)
(define-key pdf-view-mode-map (kbd "<next>") 'pdf-continuous-next-page)
;; (define-key pdf-view-mode-map  (kbd "M-<") #'pdf-cscroll-view-goto-page)
(define-key pdf-view-mode-map  (kbd "M-g g") #'pdf-cscroll-view-goto-page)
(define-key pdf-view-mode-map  (kbd "M-g M-g") #'pdf-cscroll-view-goto-page)
(define-key pdf-view-mode-map  (kbd "M-<") #'pdf-cscroll-first-page)
(define-key pdf-view-mode-map  (kbd "M->") #'pdf-cscroll-last-page)
(define-key pdf-view-mode-map  [remap forward-char] #'pdf-cscroll-image-forward-hscroll)
(define-key pdf-view-mode-map  [remap right-char] #'pdf-cscroll-image-forward-hscroll)
(define-key pdf-view-mode-map  [remap backward-char] #'pdf-cscroll-image-backward-hscroll)
(define-key pdf-view-mode-map  [remap left-char] #'pdf-cscroll-image-backward-hscroll)
(define-key pdf-view-mode-map  "T" #'pdf-cscroll-toggle-mode-line)
(define-key pdf-view-mode-map  "M" #'pdf-cscroll-toggle-narrow-mode-line)
(define-key pdf-view-mode-map (kbd "q") '(lambda ()  (interactive) (pdf-continuous-scroll-mode -1)))
(define-key pdf-view-mode-map  "Q" #'pdf-cscroll-kill-buffer-and-windows)
(define-key pdf-view-mode-map  (kbd "C-c C-a l") #'pdf-cscroll-annot-list-annotations)

(when (boundp 'spacemacs-version)
  (evil-define-key 'evilified pdf-view-mode-map
    "j" #'pdf-continuous-scroll-forward
    (kbd "<mouse-5>") #'pdf-continuous-scroll-forward
    "k" #'pdf-continuous-scroll-backward
    (kbd "<mouse-4>") #'pdf-continuous-scroll-backward
    "J" #'pdf-continuous-next-page
    "K" #'pdf-continuous-previous-page
    ;; (kbd "C-j") #'pdf-view-scroll-up-or-next-page
    ;; (kbd "C-k") #'pdf-view-scroll-down-or-previous-page
    (kbd "g t") #'pdf-view-goto-page
    (kbd "g g") #'pdf-cscroll-first-page
    "G" #'pdf-cscroll-last-page
    ;; "M" #'pdf-cscroll-toggle-mode-line
    ;; "q" #'pdf-cscroll-kill-buffer-and-windows
    ;; "l" #'pdf-cscroll-image-forward-hscroll
    ;; "h" #'pdf-cscroll-image-backward-hscroll
    ))

(provide 'pdf-continuous-scroll-mode)
