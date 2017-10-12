;;; haskell-drhaskell.el --- simplistic interaction mode with drhaskell

;; Adapted to work with drhaskell. Original creator:

;; Copyright 2004, 2005, 2006, 2007  Free Software Foundation, Inc.
;; Copyright 1998, 1999  Guy Lapalme

;; Hugs interpreter for Haskell developped by 
;;    The University of Nottingham and Yale University, 1994-1997.
;;    Web: http://www.haskell.org/hugs.
;; In standard Emacs terminology, this would be called
;;    inferior-hugs-mode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; Installation:
;;
;; To use with the Haskell mode of
;;        Moss&Thorn <http://www.haskell.org/haskell-mode>
;; add this to .emacs:
;;
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-drhaskell)
;;
;;    `haskell-drhaskell-hook' is invoked in the *drhaskell* once it is started.
;;
;;; All functions/variables start with
;;; `(turn-(on/off)-)haskell-drhaskell' or `haskell-drhaskell-'.

(defgroup haskell-drhaskell nil
  "Major mode for interacting with an inferior Drhaskell session."
  :group 'haskell
  :prefix "haskell-drhaskell-")

(defun turn-on-haskell-drhaskell ()
  (interactive)
  "Turn on Haskell interaction mode with a Drhaskell interpreter running in an
another Emacs buffer named *drhaskell*.
Maps the followind commands in the haskell keymap.
     \\[haskell-drhaskell-load-file]
       to save the current buffer and load it by sending the :load command
       to Drhaskell.
     \\[haskell-drhaskell-reload-file]
       to send the :reload command to Drhaskell without saving the buffer.
     \\[haskell-drhaskell-show-drhaskell-buffer]
       to show the Drhaskell buffer and go to it.
     \\[turn-on-haskell-drhaskell-linter] to start the linter
     \\[haskell-drhaskell-set-level] to set the linter's level manually"
  (setq haskell-doc-show-prelude nil)
  (flycheck-mode -1)
  (local-set-key "\C-c\C-s" 'haskell-drhaskell-start-process)
  (local-set-key "\C-c\C-l" 'haskell-drhaskell-load-file)
  (local-set-key "\C-c\C-r" 'haskell-drhaskell-reload-file)
  (local-set-key "\C-c\C-b" 'haskell-drhaskell-show-drhaskell-buffer)
  (local-set-key "\C-c\M-l" 'turn-on-haskell-drhaskell-linter)
  (local-set-key "\C-c\M-1" '(lambda () "Set DrHaskell level to 1" (interactive)
                               (haskell-drhaskell-set-level 1)))
  (local-set-key "\C-c\M-2" '(lambda () "Set DrHaskell level to 2" (interactive)
                               (haskell-drhaskell-set-level 2)))
  (local-set-key "\C-c\M-3" '(lambda () "Set DrHaskell level to 3" (interactive)
                               (haskell-drhaskell-set-level 3)))
  (local-set-key "\C-c\M-4" '(lambda () "Set DrHaskell level to 4" (interactive)
                               (haskell-drhaskell-set-level 4)))
  (message "DrHaskell is enabled"))

(defun turn-on-haskell-drhaskell-linter ()
  (interactive)
  "Turn on DrHaskell linter"
  (flycheck-mode)
  (setq flycheck-haskell-hlint-executable "drhaskell-lint")
  (unless (memq 'haskell-ghc flycheck-disabled-checkers)
    (push 'haskell-ghc flycheck-disabled-checkers))
  (message "DrHaskell linter is enabled")
  (flycheck-buffer)
  )

(defun turn-off-haskell-drhaskell ()
  (interactive)
  "Turn off Haskell interaction mode with a DrHaskell interpreter within a buffer."
  (local-unset-key  "\C-c\C-s")
  (local-unset-key  "\C-c\C-l")
  (local-unset-key  "\C-c\C-r")
  (local-unset-key  "\C-c\C-b")
  (local-unset-key  "\C-c\M-l")
  (local-unset-key  "\C-c\M-1")
  (local-unset-key  "\C-c\M-2")
  (local-unset-key  "\C-c\M-3")
  (local-unset-key  "\C-c\M-4")
  )

(defun haskell-drhaskell-set-level (level)
  "Set DrHaskell level"
  (interactive)
  (setq flycheck-hlint-args (list (concat "--hint=l" (int-to-string level))))
  (message "Set DrHaskell level to %s" (int-to-string level))
  (flycheck-buffer))

(define-derived-mode haskell-drhaskell-mode comint-mode "Haskell Drhaskell"
;; called by haskell-drhaskell-start-process,
;; itself called by haskell-drhaskell-load-file
;; only when the file is loaded the first time
  "Major mode for interacting with an inferior Drhaskell session.

The commands available from within a Haskell script are:
     \\<haskell-mode-map>\\[haskell-drhaskell-load-file]
       to save the current buffer and load it by sending the :load command
       to Drhaskell.
     \\[haskell-drhaskell-reload-file]
       to send the :reload command to Drhaskell without saving the buffer.
     \\[haskell-drhaskell-show-drhaskell-buffer]
       to show the Drhaskell buffer and go to it.

\\<haskell-drhaskell-mode-map>
Commands:
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands,
imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the comint or its current
subjob if any.
\\[comint-stop-subjob] stops, likewise.
 \\[comint-quit-subjob] sends quit signal."
  )

;; Drhaskell-interface

(require 'comint)
(require 'shell)

(defvar haskell-drhaskell-process nil
  "The active Drhaskell subprocess corresponding to current buffer.")

(defvar haskell-drhaskell-process-buffer nil
  "*Buffer used for communication with Drhaskell subprocess for current buffer.")

(defcustom haskell-drhaskell-program-name "drhaskell"
  "*The name of the command to start the Drhaskell interpreter."
  :type 'string
  :group 'haskell-drhaskell)

(defcustom haskell-drhaskell-program-args '("")
  "*A list of string args to send to the drhaskell process."
  :type '(repeat string)
  :group 'haskell-drhaskell)

(defvar haskell-drhaskell-load-end nil
  "Position of the end of the last load command.")

(defvar haskell-drhaskell-send-end nil
  "Position of the end of the last send command.")

(defalias 'run-drhaskell 'haskell-drhaskell-start-process)

(defun haskell-drhaskell-start-process (arg)
  "Start a Drhaskell process and invokes `haskell-drhaskell-hook' if not nil.
Prompts for a list of args if called with an argument."
  (interactive "P")
  (message "Starting `drhaskell-process' %s" haskell-drhaskell-program-name)
  (if arg
      (setq haskell-drhaskell-program-args
            (read-minibuffer "List of args for Drhaskell:"
                             (prin1-to-string haskell-drhaskell-program-args))))
  (setq haskell-drhaskell-process-buffer
        (apply 'make-comint
               "drhaskell" haskell-drhaskell-program-name nil
               haskell-drhaskell-program-args))
  (setq haskell-drhaskell-process
        (get-buffer-process haskell-drhaskell-process-buffer))
  ;; Select Drhaskell buffer temporarily
  (set-buffer haskell-drhaskell-process-buffer)
  (haskell-drhaskell-mode)
  (make-local-variable 'shell-cd-regexp)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-cd-regexp         ":cd")
  (setq shell-dirtrackp         t)
  (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil 'local)
                                ; ? or  module name in Drhaskell 1.4
  (setq comint-prompt-regexp "^.*?(.*?)> ")
    ;; comint's history syntax conflicts with Drhaskell syntax, eg. !!
  (setq comint-input-autoexpand nil)
  (run-hooks 'haskell-drhaskell-hook)
  (message "")
  )

(defun haskell-drhaskell-wait-for-output ()
  "Wait until output arrives and go to the last input."
  (while (progn
	   (goto-char comint-last-input-end)
	   (and
	    (not (re-search-forward comint-prompt-regexp nil t))
	    (accept-process-output haskell-drhaskell-process)))))

(defun haskell-drhaskell-send (&rest string)
  "Send `haskell-drhaskell-process' the arguments (one or more strings).
A newline is sent after the strings and they are inserted into the
current buffer after the last output."
  ;; Wait until output arrives and go to the last input.
  (haskell-drhaskell-wait-for-output)
  ;; Position for this input.
  (goto-char (point-max))
  (apply 'insert string)
  (comint-send-input)
  (setq haskell-drhaskell-send-end (marker-position comint-last-input-end)))

(defun haskell-drhaskell-go (load-command cd)
  "Save the current buffer and load its file into the Drhaskell process.
The first argument LOAD-COMMAND specifies how the file should be
loaded: as a new file (\":load \") or as a reload (\":reload \").

If the second argument CD is non-nil, change the Haskell-Drhaskell process to the
current buffer's directory before loading the file.

If the variable `haskell-drhaskell-command' is set then its value will be sent to
the Drhaskell process after the load command.  This can be used for a
top-level expression to evaluate."
  (hack-local-variables) ;; In case they've changed
  (save-buffer)
  (let ((file (if (string-equal load-command ":load ")
                  buffer-file-name
                  ""))
	(dir (expand-file-name default-directory))
	(cmd (and (boundp 'haskell-drhaskell-command)
		  haskell-drhaskell-command
		  (if (stringp haskell-drhaskell-command)
		      haskell-drhaskell-command
		    (symbol-name haskell-drhaskell-command)))))
    (if (and haskell-drhaskell-process-buffer
	     (eq (process-status haskell-drhaskell-process) 'run))
	;; Ensure the Drhaskell buffer is selected.
	(set-buffer haskell-drhaskell-process-buffer)
      ;; Start Haskell-Drhaskell process.
      (haskell-drhaskell-start-process nil))
 
    (if cd (haskell-drhaskell-send (concat ":cd " dir)))
    ;; Wait until output arrives and go to the last input.
    (haskell-drhaskell-wait-for-output)
    (haskell-drhaskell-send load-command file)
    ;; Error message search starts from last load command.
    (setq haskell-drhaskell-load-end (marker-position comint-last-input-end))
    (if cmd (haskell-drhaskell-send cmd))
    ;; Wait until output arrives and go to the last input.
    (haskell-drhaskell-wait-for-output)))

(defun haskell-drhaskell-load-file (cd)
  "Save a drhaskell buffer file and load its file.
If CD (prefix argument if interactive) is non-nil, change the Drhaskell
process to the current buffer's directory before loading the file.
If there is an error, set the cursor at the error line otherwise show
the Drhaskell buffer."
  (interactive "P")
  (haskell-drhaskell-gen-load-file ":load " cd)
  )

(defun haskell-drhaskell-reload-file (cd)
  "Save a drhaskell buffer file and load its file.
If CD (prefix argument if interactive) is non-nil, change the Drhaskell
process to the current buffer's directory before loading the file.
If there is an error, set the cursor at the error line otherwise show
the Drhaskell buffer."
  (interactive "P")
  (haskell-drhaskell-gen-load-file ":reload " cd)
  )

(defun haskell-drhaskell-gen-load-file (cmd cd)
  "Save a drhaskell buffer file and load its file or reload depending on CMD.
If CD is non-nil, change the process to the current buffer's directory
before loading the file. If there is an error, set the cursor at the
error line otherwise show the Drhaskell buffer."
  (save-excursion (haskell-drhaskell-go cmd cd))
  ;; Ensure the Drhaskell buffer is selected.
  (set-buffer haskell-drhaskell-process-buffer)
  ;; Error message search starts from last load command.
  (goto-char haskell-drhaskell-load-end)
  (if (re-search-forward
       "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):.*?\n" nil t)
      (let ((efile (buffer-substring (match-beginning 1) (match-end 1)))
	          (eline (if (match-beginning 2)
                       (string-to-int (buffer-substring (match-beginning 2)
                                                        (match-end 2)))))
            (ecolumn (if (match-beginning 3)
                         (string-to-int (buffer-substring (match-beginning 3)
                                                        (match-end 3)))))
            (emesg (buffer-substring (point)
                                     (save-excursion (end-of-line) (point)))))
        (pop-to-buffer  haskell-drhaskell-process-buffer) ; show *drhaskell* buffer
        ;; Jumps to first error, currently disabled
        ;; (goto-char (point-max))
        ;; (recenter)
        ;; (if (file-exists-p efile)
        ;;     ;;(and (file-exists-p efile)
        ;;     ;;     (not (string-match-p (regexp-quote ".drhaskell") efile)))
        ;;     (progn (find-file-other-window efile)
        ;;            (if eline (goto-line eline))
        ;;            (if ecolumn (move-to-column ecolumn))
        ;;            (recenter)))
        (message "DrHaskell found an error in %s: %s" (file-name-nondirectory efile) emesg)
        )
    (pop-to-buffer  haskell-drhaskell-process-buffer) ; show *drhaskell* buffer
    (goto-char (point-max))
    (message "There were no errors.")
    (recenter 2)                        ; show only the end...
    )
  )

(defun haskell-drhaskell-show-drhaskell-buffer ()
  "Goes to the Drhaskell buffer."
  (interactive)
  (if (or (not haskell-drhaskell-process-buffer)
          (not (buffer-live-p haskell-drhaskell-process-buffer)))
      (haskell-drhaskell-start-process nil))
  (pop-to-buffer  haskell-drhaskell-process-buffer)
  )

(provide 'haskell-drhaskell)

;; arch-tag: c2a621e9-d743-4361-a459-983fbf1d4589
;;; haskell-drhaskell.el ends here
