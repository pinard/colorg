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

;; colorg is a real-time collaborative editing tool meant for Emacs
;; Org mode users.  See https://github.com/pinard/colorg/wiki/.

;; At this stage, the tool is aiming simplicity: the idea is to
;; quickly get a working prototype.  All collaborating users are
;; connected to a common server, communications do not try to be fully
;; asynchronous, so users might experience delays.  Further
;; experimentation, tuning and development may follow.

;;; Code:

;; The code is organized into pages, grouping declarations by topic.
;; Such pages are introduced by a form feed and a topic description.

;;; User parameterization.

(defvar colorg-user-nickname (user-login-name)
  "Name by which this user is identified to other collaborators.")

(defvar colorg-accept-timeout 5
  "Number of seconds to wait after the colorg server.")

(defvar colorg-idle-timeout 1
  "Number of quiescent second before polling the colorg server.")

(defvar colorg-hue-bias (progn (random t) (* 0.00001 (random 100000)))
  "Bias for hue, so colors are never predictable between Emacs sessions.
You may change that to a floating constant between 0.0 and 1.0 (excluded)
if you always want the same colors for the same user numbers.")

(defvar colorg-notification-timeout 4
  "Number of seconds to keep each notification displayed.
Instead of a number, the value 'ask waits for the user to type Enter.
The special value 'org rather requests Org notifications.")

(defvar colorg-notification-beep t
  "Use nil to inhibit notification sounds.")

;;; Activation and deactivation.

(defvar colorg-data nil
  "List of (BUFFER RESOURCE SERVER) triplets.
A monitored BUFFER is associated to a RESOURCE number and a SERVER connection.
Both are needed: as each server uses it own numbering, numbers may clash.
This COLORG-DATA structure replaces buffer-local variables which would be
more attractive, if only major modes did not tamper with them unexpectedly.")

(defvar colorg-my-login-number nil
  "The login number for the local user in this buffer.")

(define-minor-mode colorg-mode
  "Collaborative editing mode.
See https://github.com/pinard/colorg/wiki/ for more information."
  nil " co" nil
  (if colorg-mode
      (condition-case err
          (let ((server (colorg-select-server)))
            (save-excursion
              (set-buffer server)
              (setq colorg-my-login-number co-user))
            (push (list (current-buffer) (colorg-associate-resource) server)
                  colorg-data))
        (error (let ((data (assq (current-buffer) colorg-data)))
                 (when data
                   (colorg-ask-server (list 'leave (nth 1 data)))))
               (setq colorg-mode nil)
               (error "Error in colorg activation: %s"
                      (error-message-string err))))
    (let ((data (assq (current-buffer) colorg-data)))
      (when data
        (colorg-ask-server (list 'leave (nth 1 data)))))
    (setq colorg-data (delq (assq (current-buffer) colorg-data)
                            colorg-data))))

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

(colorg-global-enable)

;;; Command processing.

(defun colorg-associate-resource ()
  "Select a resource interactively, then return its number.
If the resource already exists, associate the current buffer with
the resource if the contents match.  If they do not match and the
buffer is empty, download the resource in the buffer.  Otherwise,
create a new resource and upload its contents from the buffer."
  (let* ((pairs (colorg-ask-server 'resources))
         (name (and pairs
                    (completing-read "Existing resource? " pairs nil t)))
         resource)
    (if (and name (not (string-equal name "")))
        (let ((md5sum
               (let ((output (generate-new-buffer " *md5sum*")))
                 (unwind-protect
                     (progn
                       (call-process-region
                        (point-min) (point-max) "md5sum" nil output)
                       (save-excursion
                         (set-buffer output)
                         (goto-char (point-max))
                         (skip-chars-backward "- \n")
                         (buffer-substring-no-properties (point-min) (point))))
                   (kill-buffer output)))))
          (setq resource (car (colorg-ask-server (list 'join name md5sum)))))
      (setq name (read-string "New resource name? " nil nil (buffer-name)))
      (when (assoc name pairs)
        (error "This resource already exists."))
      (setq resource (car (colorg-ask-server (list 'create name))))
      (push (list 'alter resource (point-min) (point-min)
                  (buffer-substring-no-properties (point-min) (point-max)))
            colorg-outgoing-list))
    resource))

(defun colorg-select-resource ()
  "Select a resource interactively, return its number, else nil."
  (let* ((pairs (colorg-ask-server 'resources))
         (name (completing-read "Which resource? " pairs nil t)))
    (and name (cadr (assoc name pairs)))))

(defun colorg-select-user ()
  "Select a user interactively, return its number, else nil."
  (let* ((pairs (colorg-ask-server 'users))
         (name (completing-read "Which user? " pairs nil t)))
    (and name (cadr (assoc name pairs)))))

(defun colorg-send-message (text)
  (interactive)
  ;; FIXME: implement!
  )

(defun colorg-ask-server (command)
  "Send COMMAND to server, receive and process reply, then return values."
  (colorg-reply-handler (colorg-round-trip command)))

(defun colorg-reply-handler (command)
  "Process COMMAND as received from the colorg server, then return values."
  (let (action arguments values)
    (if (stringp command)
        (setq action (intern command)
              arguments nil)
      (setq action (intern (car command))
            arguments (cdr command)))
    (cond ((eq action 'alter)
           (let ((inhibit-modification-hooks t)
                 (inhibit-point-motion-hooks t)
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
               (insert-before-markers string)
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
           (setq values (append (mapcar 'colorg-reply-handler arguments))))
          ((eq action 'error)
           (let ((string (nth 0 arguments)))
             (colorg-mode 0)
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

;;; Hooks monitoring user actions.

(defun colorg-after-change-routine (start end deleted)
  "After any buffer change, tell the server about the alter to do.
These commands are accumulated and sent at regular intervals."
  (when colorg-mode
    ;; Combine a pure insert with a previous alter, whenever possible.
    (let* ((data (assq (current-buffer) colorg-data))
           (resource (cadr data))
           (server (caddr data))
           (info (and colorg-outgoing-list
                      (zerop deleted)
                      (eq (caar colorg-outgoing-list) 'alter)
                      (= (cadar colorg-outgoing-list) resource)
                      (cddar colorg-outgoing-list))))
      (if (and info (= (1- start) (+ (car info) (length (caddr info)))))
          (setcar (cddr info)
                  (concat (caddr info)
                             (buffer-substring-no-properties start end)))
        (push (list 'alter resource (1- start) (+ (1- start) deleted)
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

;;; Servers and communication.

(defvar colorg-server-buffers nil
  "Association list relating server names to server buffers.")

(defvar colorg-idle-timer nil
  "Timer used to detect the quiescence of Emacs.")

(defvar colorg-outgoing-list nil
  "Accumulated alter commands meant to be broadcasted.
This is a reversed list, the most recent command appears first.
These are sent to the server whenever Emacs gets idle for a jiffie.")

;; FIXME: Should have one such buffer per server.
(defvar colorg-buffer-name "*colorg*")
(defvar colorg-process nil)
(defvar colorg-buffer nil)

(require 'json)

(defun colorg-select-server ()
  "Select a server interactively, and return its buffer.
If the server does not exist yet, create it."
  (let* ((name (completing-read "Which server? " colorg-server-buffers))
         (pair (assoc name colorg-server-buffers)))
    (unless pair
      (when (string-equal name "")
        (cond ((not colorg-server-buffers)
               (setq name "local"))
              ((= (length colorg-server-buffers) 1)
               (setq pair (car colorg-server-buffers)
                     name (car pair)))
              (t (error "Many servers, please select one."))))
      (unless pair
        (let ((host (read-string (format "Server %s host? " name)
                                 "localhost"))
              (port (string-to-number
                     (read-string (format "Server %s port? " name)
                                  "7997")))
              (buffer (get-buffer-create (format " *colorg %s*" name))))
          (save-excursion
            (set-buffer buffer)
            (set (make-local-variable 'co-name) name)
            (set (make-local-variable 'co-process)
                 (open-network-stream name buffer host port))
            (set-process-query-on-exit-flag co-process nil)
            (setq colorg-buffer buffer
                  colorg-process co-process)
            (set (make-local-variable 'co-alist) nil)
            (set (make-local-variable 'co-outgoing) nil)
            (set (make-local-variable 'co-user)
                 (car (colorg-ask-server
                       (list 'login colorg-user-nickname)))))
          (setq pair (cons name buffer))
          (push pair colorg-server-buffers))))
    (cdr pair)))

(defun colorg-round-trip (data)
  (save-excursion
    (set-buffer colorg-buffer)
    (erase-buffer)
    (process-send-string nil (concat (json-encode data) "\n"))
    (while (not (search-forward "\n" nil t))
      (goto-char (point-max))
      (let ((here (point)))
        (accept-process-output colorg-process colorg-accept-timeout)
        (when (= here (point-max))
          (colorg-mode 0)
          (error "colorg disabled: server does not seem to reply.")))
      (goto-char (point-min)))
    (goto-char (point-min))
    (let ((json-array-type 'list))
      (json-read))))

;;; Coloration matters.

(defconst colorg-phi (* 0.5 (1+ (sqrt 5)))
  "Golden ratio.")

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

;;; Notifications.

(defvar colorg-notification-buffer-name "*colorg-notification*"
  "Name of colorg notification buffer.")

(defvar colorg-notification-is-displayed nil
  "A notification is currently displayed.")

(defvar colorg-pending-notifications nil
  "Notifications waiting to be displayed.")

(defun colorg-notify (text)
  "Manage to notify the user with TEXT as a message."
  (message "colorg-notify: %s" text)
  (message "")
  (cond ((eq colorg-notification-timeout 'org)
         (org-notify text colorg-notification-beep))
        ((eq colorg-notification-timeout 'ask)
         (colorg-show-notification text)
         (read-string "Got it? [Enter] ")
         (colorg-hide-notification))
        (t (add-to-list 'colorg-pending-notifications text t)
           (unless colorg-notification-is-displayed
             (colorg-advance-notifications)))))

(defun colorg-advance-notifications ()
  (when colorg-notification-is-displayed
    (colorg-hide-notification))
  (when colorg-pending-notifications
    (colorg-show-notification (pop colorg-pending-notifications))
    (run-at-time colorg-notification-timeout nil
                 'colorg-advance-notifications)))

(defun colorg-show-notification (text)
  "Display TEXT as a notification, in a separate buffer."
  ;; Adapted from appt.el.
  (when colorg-notification-beep
    (beep))
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
          buffer-undo-list t
          mode-line-format nil)
    (erase-buffer)
    (insert text)
    (let ((window-min-height 1))
       (shrink-window-if-larger-than-buffer (get-buffer-window buffer t)))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (raise-frame (selected-frame))
    (select-window this-window))
  (setq colorg-notification-is-displayed t))

(defun colorg-hide-notification ()
  "Function called to undisplay the notification message."
  ;; Adapted from appt.el.
  (setq colorg-notification-is-displayed nil)
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

(provide 'colorg)
;;; colorg.el ends here
