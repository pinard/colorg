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
  "Accumulated change instructions meant to be broadcasted.
These are sent to the server whenever Emacs gets idle for a jiffie.")

(defun colorg-after-change-routine (start end deleted)
  "After any buffer change, tell the server about the change to do.
These instructions are accumulated and sent at regular intervals."
  (when colorg-current-resource
    ;; Combine a pure insert with a previous change, whenever possible.
    (let ((info (and colorg-outgoing-list
                     (zerop deleted)
                     (eq (caar colorg-outgoing-list) 'change)
                     (cdar colorg-outgoing-list))))
      (if (and info (= start (+ (car info) (length (caddr info)))))
          (setcar (cddr info)
                  (concat (caddr info) (buffer-substring start end)))
        (push (list 'change start (+ start deleted) (buffer-substring start end))
              colorg-outgoing-list)))))

(defun colorg-before-change-routine (start end)
  "Before any buffer change, instruct the server to assert previous contents.
These instructions are accumulated and sent at regular intervals.
This is merely a debugging feature, which may be inhibited to get some speed."
  (when colorg-current-resource
    (unless (= start end)
      (push (list 'assert start end (buffer-substring start end))
            colorg-outgoing-list))))

(require 'json)

(defun colorg-idle-routine ()
  "Whenever Emacs gets idle, round-trip with the synchronization server.
We push out accumulated instructions.  Then, we get externally triggered
change instructions from the server and execute them all."
  (let ((outgoing (if colorg-outgoing-list
                      (cons 'poll (nreverse colorg-outgoing-list))
                    'poll)))
    (message (json-encode outgoing)))
  (setq colorg-outgoing-list nil)
  (sleep-for 0.5))

;;; Communication protocol.

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
  (setq colorg-current-resource t)
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

(provide 'colorg)
;;; colorg.el ends here
