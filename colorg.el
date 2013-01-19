;;; colorg.el --- Emacs Lisp part of real-time collaboration for Org mode.

;; Copyright © 2013 Progiciels Bourbeau-Pinard inc.

;; Author: François Pinard <pinard@iro.umontreal.ca>
;; Maintainer: François Pinard <pinard@iro.umontreal.ca>
;; Created: 2013
;; Version: 0.0
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

;;; Commentary:

;; ColOrg is a real-time collaborative editing tool meant for Emacs
;; Org mode users.  See https://github.com/pinard/ColOrg/wiki/.

;; At this stage, the tool is aiming simplicity: the
;; idea is to quickly get a working prototype.  All collaborating
;; users are connected to a common server, communications do not try
;; to be fully asynchronous, so users might experience delays.
;; Further experimentation, tuning and development may follow.

;;; Code:

;; The code is organized into pages, grouping declarations by topic.
;; Such pages are introduced by a form feed and a topic description.

;;; Essential hooks

(defvar colorg-current-resource nil
  "Description of the synchronized resource for the current buffer.

The value is t when the buffer is synchronized, nil otherwise.
Currently, the code takes care of a single resource on a single
server.  This variable is meant to contain a description of the
server and the resource on that server, for when the tool will be
able to relate many synchronized buffers at once to resources
kept on one of more servers.")

(make-variable-buffer-local 'colorg-current-resource)

(defvar colorg-idle-timer nil
  "Timer used to detect the quiescence of Emacs.")

(defvar colorg-outgoing-list nil
  "Accumulated alter commands meant to be broadcasted.
These are sent to the server whenever Emacs gets idle for a jiffie.")

(defun colorg-after-change-routine (start end deleted)
  "After any buffer change, tell the server about the alter to do.
These commands are accumulated and sent at regular intervals."
  (when colorg-current-resource
    ;; Combine a pure insert with a previous alter, whenever possible.
    (let ((info (and colorg-outgoing-list
                     (zerop deleted)
                     (string-equal (caar colorg-outgoing-list) "alter")
                     (= (cadar colorg-outgoing-list) colorg-current-resource)
                     (cddar colorg-outgoing-list))))
      (if (and info (= start (+ (car info) (length (caddr info)))))
          (setcar (cddr info)
                  (concat (caddr info) (buffer-substring start end)))
        (push (list "alter" colorg-current-resource start (+ start deleted)
                    (buffer-substring start end))
              colorg-outgoing-list)))))

(defun colorg-before-change-routine (start end)
  "Before any buffer change, instruct the server to assert previous contents.
These commands are accumulated and sent at regular intervals.
This is merely a debugging feature, which may be inhibited to get some speed."
  (when colorg-current-resource
    (unless (= start end)
      (push (list "check" start end (buffer-substring start end))
            colorg-outgoing-list))))

(defun colorg-idle-routine ()
  "Whenever Emacs gets idle, round-trip with the synchronization server.
We push out accumulated commands.  Then, we get externally
triggered alter commands from the server and execute them all."
  (let* ((outgoing (if colorg-outgoing-list
                       (cons 'poll (nreverse colorg-outgoing-list))
                     'poll))
         (results (colorg-round-trip outgoing)))
    (message "%S" results)))

;;; Communication protocol.

(defvar colorg-buffer-name "*ColOrg*")
(defvar colorg-process nil)
(defvar colorg-buffer nil)

(require 'json)

(defun colorg-round-trip (data)
  (unless (processp colorg-process)
    (setq colorg-buffer (get-buffer-create colorg-buffer-name))
    (setq colorg-process
          (open-network-stream "essai" colorg-buffer "localhost" 7997)))
  (save-excursion
    (set-buffer colorg-buffer)
    (erase-buffer)
    (process-send-string nil (concat (json-encode data) "\n"))
    (while (not (search-forward "\n" nil t))
      (accept-process-output colorg-process)
      (goto-char (point-min)))
    (goto-char (point-min))
    (let ((json-array-type 'list)) (json-read))))

;;; Activation and deactivation.

(defun colorg-global-enable ()
  (interactive)
  (add-hook 'after-change-functions 'colorg-after-change-routine)
  (add-hook 'before-change-functions 'colorg-before-change-routine)
  (setq colorg-idle-timer (run-with-idle-timer 2 t 'colorg-idle-routine)))

(defun colorg-global-disable ()
  (interactive)
  (remove-hook 'after-change-functions 'colorg-after-change-routine)
  (remove-hook 'before-change-functions 'colorg-before-change-routine)
  (cancel-timer colorg-idle-timer))

(defun colorg-local-enable ()
  (interactive)
  (let ((reply (colorg-round-trip
                (list "create" (read-string "Resource name? ")))))
    (unless (string-equal (car reply) "done")
      (error "%S" reply))
    (setq colorg-current-resource (cadr reply)))
  (push (list "alter" colorg-current-resource (point-min) (point-min)
              (buffer-substring (point-min) (point-max)))
        colorg-outgoing-list)
  (message "ColOrg enabled."))

(defun colorg-local-disable ()
  (interactive)
  (setq colorg-current-resource nil)
  (message "ColOrg disabled."))

(defun colorg-toggle-local ()
  (interactive)
  (if colorg-current-resource
      (colorg-local-disable)
    (colorg-local-enable)))

(colorg-global-enable)

;;; Coloration matters.

(defvar colorg-test-ordinal 0)

(defun colorg-test (start end)
  (interactive "r")
  (colorg-colorize start end colorg-test-ordinal)
  (setq colorg-test-ordinal (1+ colorg-test-ordinal)))

(defconst colorg-phi (* 0.5 (1+ (sqrt 5)))
  "Golden ratio.")

(defvar colorg-hue-bias (progn (random t) (* 0.00001 (random 100000)))
  "Bias for hue, so colors are never predictable.")

(defvar colorg-overlays nil
  "Association between ordinals and overlays.")

(defun colorg-colorize (start end ordinal)
  "Highlight region from START to END with a color tied to ORDINAL.
The first time an ordinal appears, automatically select a color for it.
Else, first remove the previous highlight made for that ordinal."
  (let ((pair (assoc ordinal colorg-overlays)))
    (if pair
        (move-overlay (cdr pair) start end)
      (let ((overlay (make-overlay start end))
            (rgb (colorg-hsv-to-rgb
                  (+ colorg-hue-bias (* colorg-phi ordinal)) 0.25 1.0))
            (symbol (intern (format "colorg-face-%d" ordinal))))
        (set-face-background (make-face symbol)
                             (format "#%02x%02x%02x"
                                     (floor (* 255.0 (nth 0 rgb)))
                                     (floor (* 255.0 (nth 1 rgb)))
                                     (floor (* 255.0 (nth 2 rgb)))))
        (overlay-put overlay 'face symbol)
        (push (cons ordinal overlay) colorg-overlays)))))

(defun colorg-hsv-to-rgb (hue saturation value)
  "Convert an HSV color to RGB.
Only the fractional part of HUE is used, so it gets within [0.0, 1.0).
SATURATION and VALUE are both to be given within [0.0, 1.0].
Returns a list (RED GREEN BLUE), each within [0.0, 1.0].
Adapted from Adrian Aichner code, see http://emacswiki.org/emacs/hsv2rgb.el."
  (setq saturation (float saturation)
        value (float value))
  (let* ((6*hue (* hue 6.0))
         (index (mod (floor 6*hue) 6))
         (fraction (mod 6*hue 1.0))
         (x (* value (- 1.0 saturation)))
         (y (* value (- 1.0 (* fraction saturation))))
         (z (* value (- 1.0 (* (- 1.0 fraction) saturation)))))
    (cond ((= index 0) (list value z x))
          ((= index 1) (list y value x))
          ((= index 2) (list x value z))
          ((= index 3) (list x y value))
          ((= index 4) (list z x value))
          (t (list value x y)))))

(provide 'colorg)
;;; colorg.el ends here
