;;; chromatext.el --- Package description (don't include the word "Emacs")  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 First Last

;; Author: First Last <name@example.com>
;; URL: http://example.com/chromatext.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: something

;;; Commentary:

;; This is my package.  It is nice.  You should try it.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + foo
;; + bar

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'chromatext)

;;;; Usage

;; Run one of these commands:

;; `chromatext-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `chromatext' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'color)

(require 'dash)

;;;; Customization

(defgroup chromatext nil
  "Settings for `chromatext'."
  :link '(url-link "http://example.com/chromatext.el"))

(defcustom chromatext-color-pairs (list (list (face-foreground 'default)
                                              (color-lighten-name (face-foreground 'default)
                                                                  50)))
  "Pairs of colors which are used to colorize text.
Each color should be a string that `color-name-to-rgb' accepts.
Each pair is duplicated and reversed before the next pair."
  :type '(repeat (list string string))
  :set (lambda (option value)
         (setq value (-cycle
                      (cl-loop for pair in value
                               append (list pair
                                            (reverse pair)))))
         (set-default option value)))

;;;; Functions

;;;;; Commands

;;;###autoload
(defun chromatext-chromatize-region (&optional start end color-pairs)
  "Chromatize lines between START and END using COLOR-PAIRS.
If START and END are nil, operate on the whole buffer.  If
COLOR-PAIRS is nil, use default pairs."
  (interactive)
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (start (or start
                   (when (use-region-p)
                     (region-beginning))
                   (point-min)))
        (end (or end
                 (when (use-region-p)
                   (region-end))
                 (point-max))))
    (chromatext--chromatize-lines start end color-pairs)
    (font-lock-flush start end)))

;;;###autoload
(defun chromatext-chromatize-org-entry (&optional color-pairs)
  "Chromatize lines in current Org entry.
If COLOR-PAIRS is nil, use default pairs."
  (interactive)
  (let ((buffer-undo-list t))
    (chromatext--chromatize-lines (org-entry-beginning-position) (org-entry-end-position)
                                  color-pairs)))

;;;###autoload
(cl-defun chromatext-remove-colors (&optional (start (point-min)) (end (point-max)))
  "Remove `chromatize' colors between START and END.
If START and END are nil, operate on the whole buffer."
  (interactive)
  (let ((buffer-undo-list t))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (unless (get-text-property (point) :chromatext-chromatized)
          (goto-char (next-single-property-change start :chromatext-chromatized)))
        (cl-loop until (eobp)
                 for start = (point)
                 for end = (or (next-single-property-change start :chromatext-chromatized)
                               (point-max))
                 do (let ((face (get-text-property (point) 'face)))
                      (remove-list-of-text-properties start end '(:chromatext-chromatized face))
                      (add-text-properties start end (list 'face (cddr face)))
                      (goto-char end)))))))

;;;;; Support

(cl-defun chromatext--chromatize-lines
    (start end &optional color-pairs (advance-fn (lambda () (line-move-visual 1))))
  "Chromatize lines between START and END with COLOR-PAIRS.
Advance to the next line with ADVANCE-FN.  COLOR-PAIRS should be
an infinitely cycling list of color pairs."
  (let ((color-pairs (or color-pairs
                         chromatext-color-pairs)))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (cl-loop for (start-color end-color) in color-pairs
                 until (eobp)
                 do (progn
                      ;; Skip blank lines
                      (while (looking-at (rx bol eol))
                        (funcall advance-fn)))
                 do (progn
                      (chromatext--chromatize-words (point-at-bol) (point-at-eol) start-color end-color)
                      (funcall advance-fn)))))))

(defun chromatext--chromatize-words (start end start-color end-color)
  "Apply colors from START-COLOR to END-COLOR to words between START and END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (cl-loop with colors = (color-gradient (color-name-to-rgb start-color) (color-name-to-rgb end-color)
                                             (chromatext--count-words))
               for color in colors
               until (eobp)
               for start = (point)
               for end = (progn
                           (forward-word 1)
                           (point))
               for color = (apply #'color-rgb-to-hex color)
               do (chromatext--colorize-region start end color)))))

(defun chromatext--count-words ()
  "Return number of words in buffer.
Typically used with narrowing."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (cl-loop until (eobp)
               count (forward-word 1)))))

(defun chromatext--colorize-region (start end color)
  "Apply COLOR to text between START and END."
  (add-text-properties
   start end (list :chromatext-chromatized t))
  (add-face-text-property
   start end (list :foreground color)))

;;;; Footer

(provide 'chromatext)

;;; chromatext.el ends here
