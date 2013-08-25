
;;; phi-search-mc.el --- multiple-cursors extension for phi-search
;;
;; Copyright (c) 2013 Akinori MUSHA
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/phi-search-mc.el
;; Created: 25 Aug 2013
;; Version: 1.0.20130825
;; Keywords: search cursors

;;; Commentary:
;;
;; phi-search-mc.el provides the following interactive commands:
;;
;; * phi-search-mc/mark-here
;; * phi-search-mc/mark-forward
;; * phi-search-mc/mark-backward
;; * phi-search-mc/mark-all
;;
;; They serve as great way to add fake cursors at your desired points
;; using phi-search.
;;
;; Suggested key bindings are as follows:
;;
;;   (define-key phi-search-mode-map (kbd "C->") 'phi-search-mc/mark-next)
;;   (define-key phi-search-mode-map (kbd "C-<") 'phi-search-mc/mark-previous)
;;   (define-key phi-search-mode-map (kbd "C-. !") 'phi-search-mc/mark-all)

;;; Code:

(require 'phi-search)
(require 'multiple-cursors)

(eval-when-compile
  (require 'cl))

(defvar phi-search--mc/fake-cursors nil
  "Keeps a list of fake cursors that are activated after exiting phi-search.")
(make-variable-buffer-local 'phi-search--mc/fake-cursors)

(defun phi-search--mc/fake-cursor-p (ov)
  (eq (overlay-get ov 'type) 'phi-search--fake-cursor))

(defun phi-search--mc/fake-cursor-at-pos-p (pos)
  (loop for ov in (overlays-at pos)
        thereis (phi-search--mc/fake-cursor-p ov)))

(defun phi-search--mc/add-fake-cursor (pos)
  (or
   (phi-search--mc/fake-cursor-at-pos-p pos)
   (let ((ov (make-overlay pos (1+ pos) nil nil nil)))
     (overlay-put ov 'type 'phi-search--fake-cursor)
     (overlay-put ov 'face 'mc/cursor-face)
     (add-to-list 'phi-search--mc/fake-cursors ov))))

(defmacro phi-search--mc/mark-do (&rest body)
  `(progn
     (phi-search--with-target-buffer
      (when (> (mc/num-cursors) 1)
        ;; Save existing fake cursors and remove them, or they
        ;; will move when phi-search exits.
        (mc/for-each-fake-cursor
         (phi-search--mc/add-fake-cursor (overlay-start cursor)))
        (mc/remove-fake-cursors))
      ,@body)
     (add-hook 'kill-buffer-hook 'phi-search--mc/activate-fake-cursors)))

(defun phi-search--mc/activate-fake-cursors ()
  (and phi-search--target
       (phi-search--with-target-buffer
        (loop for ov in phi-search--mc/fake-cursors do
              (let ((pos (overlay-start ov)))
                (delete-overlay ov)
                (and (/= pos (point))
                     (loop for o in (overlays-at pos)
                           never (mc/fake-cursor-p o))
                     (mc/save-excursion
                      (goto-char pos)
                      (mc/create-fake-cursor-at-point)))))
        (setq phi-search--mc/fake-cursors nil)
        (mc/maybe-multiple-cursors-mode)
        ;; Prevent the fake cursors from moving via mc's post-command-hook
        (setq this-original-command nil))))

;;;###autoload
(defun phi-search-mc/mark-here ()
  "Mark the current match as fake cursor."
  (interactive)
  (phi-search--mc/mark-do
   (phi-search--mc/add-fake-cursor
    (overlay-end (nth phi-search--selection phi-search--overlays)))))

;;;###autoload
(defun phi-search-mc/mark-next ()
  "Mark the current match as fake cursor and search next item."
  (interactive)
  (phi-search-mc/mark-here)
  (phi-search-again-or-next))

;;;###autoload
(defun phi-search-mc/mark-previous ()
  "Mark the current match as fake cursor and search previous item."
  (interactive)
  (phi-search-mc/mark-here)
  (phi-search-again-or-previous))

;;;###autoload
(defun phi-search-mc/mark-all ()
  "Mark all matches as fake cursors."
  (interactive)
  (phi-search--mc/mark-do
   (dolist (ov phi-search--overlays)
     (phi-search--mc/add-fake-cursor
      (overlay-end ov)))))

(provide 'phi-search-mc)

;;; phi-search-mc.el ends here
