# mpg123-simple
a simple interface to mpg123

### Usage
Add these lines to your ".emacs.d/init.el" or ".emacs.el" .

```lisp
(require 'mpg123-simple)
(setf *mpg123-dir* "/path/to/mp3dir/") ;; default value is "~/mp3/"
```

### KeyBind

- q : quit
- h : help
- R : Repeat
- S : Shuffle
- L : Loop 

press "h" for more keybinds.

### Helm Interface

To select your playlists, helm is useful.

```lisp
(defun search-file (path match &optional ignore)
  "Searches files of a directory recursively. Returns the list of matched files. "
  (let ((result nil)
        (regexp (if ignore
                    (concat "/[.]\\{1,2\\}$" "\\|" ignore)
                  "/[.]\\{1,2\\}$")))
    (cl-labels ((rec (path)
                     (dolist (f (directory-files path t))
                       (cond
                        ((string-match match f) (push f result))
                        ((and (file-accessible-directory-p f)
                              (not (string-match regexp f)))
                         (rec f))
                        (t nil)))))
      (rec path))
    (nreverse result)))
    
(defvar helm-mpg123-playlists
  '((name . "Play List")
    (candidates . (lambda () (search-file *mpg123-dir* "\\.m3u$")))
    (action . helm-mpg123-open-playlist)))

(defun helm-mpg123-open-playlist (f) (interactive) (mpg123-mode f))

(defun helm-mpg123 () (interactive) (helm :sources (list helm-mpg123-playlists)))
```

### Anything

```
(defun search-file (path match &optional ignore)
  "Searches files of a directory recursively. Returns the list of matched files. "
  (let ((result nil)
        (regexp (if ignore
                    (concat "/[.]\\{1,2\\}$" "\\|" ignore)
                  "/[.]\\{1,2\\}$")))
    (cl-labels ((rec (path)
                     (dolist (f (directory-files path t))
                       (cond
                        ((string-match match f) (push f result))
                        ((and (file-accessible-directory-p f)
                              (not (string-match regexp f)))
                         (rec f))
                        (t nil)))))
      (rec path))
    (nreverse result)))
    
(defvar anything-mpg123-playlists
  '((name . "Play List")
    (candidates . (lambda () (search-file *mpg123-dir* "\\.m3u$")))
    (action . (("Open PlayList" . anything-mpg123-open-playlist)))))

(defun anything-mpg123 () (interactive) (anything (list anything-mpg123-playlists)))
(defun anything-mpg123-open-playlist (f) (interactive) (mpg123-mode f))
```

