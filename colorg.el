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

;;; Main declarations

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
This is a reversed list, the most recent command appears first.
These are sent to the server whenever Emacs gets idle for a jiffie.")

(defvar colorg-accept-timeout 5
  "Number of seconds to wait after the ColOrg server.")

(defvar colorg-idle-timeout 2
  "Number of quiescent second before polling the ColOrg server.")

(defvar colorg-notification-timeout 3
  "Number of seconds to keep a notification displayed.")

;;; Main hooks

(defun colorg-after-change-routine (start end deleted)
  "After any buffer change, tell the server about the alter to do.
These commands are accumulated and sent at regular intervals."
  (when colorg-current-resource
    ;; Combine a pure insert with a previous alter, whenever possible.
    (let ((info (and colorg-outgoing-list
                     (zerop deleted)
                     (eq (caar colorg-outgoing-list) 'alter)
                     (= (cadar colorg-outgoing-list) colorg-current-resource)
                     (cddar colorg-outgoing-list))))
      (if (and info (= (1- start) (+ (car info) (length (caddr info)))))
          (setcar (cddr info)
                  (concat (caddr info)
                             (buffer-substring-no-properties start end)))
        (push (list 'alter colorg-current-resource
                    (1- start) (+ (1- start) deleted)
                    (buffer-substring-no-properties start end))
              colorg-outgoing-list)))))

(defun colorg-idle-routine ()
  "Whenever Emacs gets idle, round-trip with the synchronization server.
We push out accumulated commands.  Then, we get externally
triggered alter commands from the server and execute them all."
  (let ((outgoing colorg-outgoing-list))
    (setq colorg-outgoing-list nil)
    (let ((values
           (save-match-data
             (colorg-ask-server (if outgoing
                                    (cons 'poll (nreverse outgoing))
                                  'poll)))))
      ;;(timer-set-idle-time colorg-idle-timer colorg-idle-timeout)
      values)))

;;; Local actions.

(defun colorg-ask-server (command)
  "Send COMMAND to server, receive and process reply, then return values."
  (colorg-process (colorg-round-trip command)))

(defun colorg-process (command)
  (let (action arguments values)
    (if (stringp command)
        (setq action (intern command)
              arguments nil)
      (setq action (intern (car command))
            arguments (cdr command)))
    (cond ((eq action 'alter)
           (let ((inhibit-point-motion-hooks t)
                 (resource (nth 0 arguments))
                 (start (1+ (nth 1 arguments)))
                 (end (1+ (nth 2 arguments)))
                 (string (nth 3 arguments))
                 (user (nth 4 arguments)))
             (save-excursion
               ;; FIXME: Switch to the proper resource.
               (delete-region start end)
               (goto-char start)
               (when (numberp string)
                 (setq string (make-string string '?')))
               (insert string)
               (colorg-colorize start (+ start (length string)) user))))
          ((eq action 'chat)
           (let ((user (nth 0 arguments))
                 (string (nth 1 arguments)))
             (colorg-notify
              (concat
               (propertize (format "User %s:" user)
                           'face (colorg-face-name user) 'weight "bold")
               " " string))))
          ((eq action 'done)
           (setq values arguments))
          ((eq action 'exec)
           (setq values (append (mapcar 'colorg-process arguments))))
          ((eq action 'error)
           (let ((string (nth 0 arguments)))
             (colorg-local-disable)
             (colorg-notify
              (concat
               (propertize "Error (disabling): " 'face 'font-lock-warning-face)
               string))))
          ((eq action 'warn)
           (let ((string (nth 0 arguments)))
             (colorg-notify
              (concat
               (propertize "Warning: " 'face 'font-lock-warning-face)
               string))))
          (t (debug)))
    values))

(defvar colorg-notification-buffer-name "*colorg-notification*"
  "Name of ColOrg notification buffer.")

(defun colorg-notify (text)
  (message "colorg-notify: %s" text)
  (colorg-show-notification text)
  ;; FIXME: (beep) to be made programmable.
  (run-at-time colorg-notification-timeout nil
               'colorg-hide-notification))

(defun colorg-show-notification (text)
  "Display TEXT as a notification, in a separate buffer."
  ;; Adapted from appt.el.
  (let ((this-window (selected-window))
        (buffer (get-buffer-create colorg-notification-buffer-name)))
    ;; Make sure we're not in the minibuffer before splitting the window.
    (when (minibufferp)
      (other-window 1)
      (and (minibufferp) (display-multi-frame-p) (other-frame 1)))
    (if (cdr (assq 'unsplittable (frame-parameters)))
        ;; In an unsplittable frame, use something somewhere else.
	(progn
	  (set-buffer buffer)
	  (display-buffer buffer))
      (unless (or (special-display-p (buffer-name buffer))
                  (same-window-p (buffer-name buffer)))
        ;; By default, split the bottom window and use the lower part.
        (appt-select-lowest-window)
        ;; Split the window, unless it's too small to do so.
        (when (>= (window-height) (* 2 window-min-height))
          (select-window (split-window))))
      (switch-to-buffer buffer))
    (setq buffer-read-only nil
          buffer-undo-list t)
    (erase-buffer)
    (insert text)
    (shrink-window-if-larger-than-buffer (get-buffer-window buffer t))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (raise-frame (selected-frame))
    (select-window this-window)))

(defun colorg-hide-notification ()
  "Function called to undisplay the notification message."
  ;; Adapted from appt.el.
  (let ((window (get-buffer-window colorg-notification-buffer-name t)))
    (and window
         (or (eq window (frame-root-window (window-frame window)))
             (delete-window window))))
  (kill-buffer colorg-notification-buffer-name))

(defun colorg-select-lowest-window ()
  "Select the lowest window on the frame."
  ;; Stolen from appt.el.
  (let ((lowest-window (selected-window))
        (bottom-edge (nth 3 (window-edges)))
        next-bottom-edge)
    (walk-windows (lambda (window)
                    (when (< bottom-edge
                             (setq next-bottom-edge
                                   (nth 3 (window-edges window))))
                      (setq bottom-edge next-bottom-edge
                            lowest-window window)))
                  'except-minibuffer)
    (select-window lowest-window)))

;;; Communication protocol.

;; FIXME: Should have one such buffer per server.
(defvar colorg-buffer-name "*ColOrg*")
(defvar colorg-process nil)
(defvar colorg-buffer nil)

(require 'json)

(defun colorg-round-trip (data)
  (unless (and (processp colorg-process)
               ;; FIXME: Because I restart the server?  Not healthy!
               (eq (process-status colorg-process) 'open))
    (setq colorg-buffer (get-buffer-create colorg-buffer-name))
    (setq colorg-process
          (open-network-stream "essai" colorg-buffer "localhost" 7997)))
  (save-excursion
    (set-buffer colorg-buffer)
    (erase-buffer)
    (process-send-string nil (concat (json-encode data) "\n"))
    (while (not (search-forward "\n" nil t))
      (goto-char (point-max))
      (let ((here (point)))
        (accept-process-output colorg-process colorg-accept-timeout)
        (when (= here (point-max))
          (colorg-local-disable)
          (error "ColOrg disabled: server does not seem to reply.")))
      (goto-char (point-min)))
    (goto-char (point-min))
    (let ((json-array-type 'list)) (json-read))))

;;; Activation and deactivation.

(defun colorg-global-enable ()
  (interactive)
  (add-hook 'after-change-functions 'colorg-after-change-routine)
  (setq colorg-idle-timer
        ;; FIXME: run-with-idle-timer is the real goal, but I do not
        ;; succeed in firing it frequently enough.
        (run-with-timer 0.1 colorg-idle-timeout 'colorg-idle-routine)))

(defun colorg-global-disable ()
  (interactive)
  (remove-hook 'after-change-functions 'colorg-after-change-routine)
  (cancel-timer colorg-idle-timer))

(defun colorg-local-enable ()
  (interactive)
  (let ((
  (let ((name (read-string "Resource name? ")))
    (when (string-equal name "")
      (setq name (buffer-name)))
    (if (= (point-min) (point-max))
        (let ((values (colorg-ask-server (list 'join name md5sum))))
          (unless values
            (error "Resource could not be joined."))
          (setq colorg-current-resource (car values)))
      (let ((values (colorg-ask-server (list 'create name))))
        (unless values
          (error "Resource could not be created."))
      ((set  )q colorg-current-resource (car values))))
    (push (list 'alter colorg-current-resource (point-min) (point-min)
                (buffer-substring-no-properties (point-min) (point-max)))
          colorg-outgoing-list)
    (message "ColOrg enabled.")))

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

(defconst colorg-phi (* 0.5 (1+ (sqrt 5)))
  "Golden ratio.")

(defvar colorg-hue-bias (progn (random t) (* 0.00001 (random 100000)))
  "Bias for hue, so colors are never predictable.")

(defvar colorg-overlays nil
  "Association between keys and overlays.")

(defun colorg-colorize (start end key)
  "Highlight region from START to END with a color tied to KEY.
The first time an key appears, automatically select a color for it.
Else, first remove the previous highlight made for that key."
  (let ((pair (assoc key colorg-overlays)))
    (if pair
        (move-overlay (cdr pair) start end)
      (let ((overlay (make-overlay start end)))
        (overlay-put overlay 'face (colorg-face-name key))
        (push (cons key overlay) colorg-overlays)))))

(defun colorg-face-name (key)
  "Return the face name, as a symbol, associated with user KEY.
Create a new face if it does not exist already."
  (let ((symbol (intern (format "colorg-face-%d" key))))
    (unless (facep symbol)
      (let ((rgb (colorg-hsv-to-rgb
                  (+ colorg-hue-bias (* colorg-phi key)) 0.25 1.0)))
        (set-face-background (make-face symbol)
                             (format "#%02x%02x%02x"
                                     (floor (* 255.0 (nth 0 rgb)))
                                     (floor (* 255.0 (nth 1 rgb)))
                                     (floor (* 255.0 (nth 2 rgb)))))))
    symbol))

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
