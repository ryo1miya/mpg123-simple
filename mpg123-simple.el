;;; mpg123-simple.el --- a simple interface to `mpg123'

;;     Copyright (C) 2016 MIYAZAKI Ryoichi

;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.

;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentray :

;; This program provides `mpg123-mode' and `helm-mpg123' function.
;; 
;; To use this program, add these lines to your .emacs.d/init.el or .emacs.el:
;;     (require 'mpg123-simple)
;;     (setf *mpg123-dir* "/path/to/mp3dir/") ;; default value is "~/mp3/"

(require 'cl-lib)

(defvar *mpg123-dir* (expand-file-name "~/mp3/"))
(defvar *mpg123-buf* "*mpg123*")
(defvar *mpg123-proc* nil)
(defvar *mpg123-current-m3u* nil)

(defun mpg123-make-keylist (lst)
  (cl-loop for elt in lst collect 
        `(define-key mpg123-mode-map ,elt (lambda () (interactive) (process-send-string *mpg123-proc* ,elt)))))

(defmacro mpg123-defkey (&rest lst) `(progn ,@(mpg123-make-keylist lst)))

(defun mpg123-quit ()
  (interactive)
  (when (get-buffer-process *mpg123-buf*)
    (process-send-string *mpg123-proc* "q")
    (setf *mpg123-proc* nil)))

(defun mpg123-delete-process ()
  (when (get-buffer-process *mpg123-buf*)
    (delete-process *mpg123-proc*)))

(defun mpg123-replay ()
  (interactive)
  (mpg123-delete-process)
  (mpg123-mode *mpg123-current-m3u*))

(defun mpg123-shuffle ()
  (interactive)
  (mpg123-delete-process)
  (mpg123-mode *mpg123-current-m3u* '(4)))

(defun mpg123-loop (i)
  (interactive "nInput Track Number : ")
  (mpg123-delete-process)
  (mpg123-mode *mpg123-current-m3u* i))

(defun mpg123-mode (f &optional arg)
  (interactive 
   (list (file-name-nondirectory (read-file-name "Input PlayList : " *mpg123-dir*))
         current-prefix-arg))
  ;; initial settings
  (unless (boundp 'mpg123-mode-map)
    (setf mpg123-mode-map (make-keymap))
    (mpg123-defkey "s" " " "f" "d" "b" "p" "." "," ":" ";" ">" "<" "+" "-" "r" "v" "l" "t" "m" "h" "c" "C" "x" "X" "w" "k")
    (define-key mpg123-mode-map "q" 'mpg123-quit)
    (define-key mpg123-mode-map "R" 'mpg123-replay)
    (define-key mpg123-mode-map "S" 'mpg123-shuffle)
    (define-key mpg123-mode-map "L" 'mpg123-loop)
    (suppress-keymap mpg123-mode-map)
    (make-variable-buffer-local '*mpg123-proc*)
    (make-variable-buffer-local '*mpg123-current-m3u*))
  (when *mpg123-proc* (mpg123-delete-process))
  ;; main part
  (let ((buf (get-buffer-create *mpg123-buf*)))
    (pop-to-buffer buf)
    (comint-mode)
    (erase-buffer)
    (setf default-directory (expand-file-name *mpg123-dir*)
          major-mode 'mpg123-mode mode-name "mpg123" *mpg123-current-m3u* f
          *mpg123-proc*
          (cond ((numberp arg) ;; loop arg-th track
                 (start-process "mpg123" buf "mpg123" "--loop" "-1" "-l" (number-to-string arg) "-Cv@" f))
                ((equal arg '(4)) ;; shuffle
                 (start-process "mpg123" buf "mpg123" "-Cvz@" f))
                (t ;; normal
                 (start-process "mpg123" buf "mpg123" "-Cv@" f))))
    (set-process-filter *mpg123-proc* 'comint-output-filter)
    (use-local-map mpg123-mode-map)))

(provide 'mpg123-simple)

;; Local Variables:
;; mode: emacs-lisp
;; End:
