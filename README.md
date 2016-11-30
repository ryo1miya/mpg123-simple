# mpg123-simple
a simple interface to mpg123

### Usage
Add these lines to your ".emacs.d/init.el" or ".emacs.el" .
```
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

### Directory Structure 
If you have following directory structure, `` `helm` `` or `` `anything` `` will help you to search your playlists.

    mp3/
       | artist1/
       |        | album1/
       |        | album2/
       |        | album3/
       |
       | artist1-album1.m3u
       | artist1-album2.m3u
       | artist1-album3.m3u
       |
       | artist2/
       |        | album1/
       |        | album2/
       |
       | artist2-album1.m3u
       | artist2-album2.m3u

#### For Helm
```
(defvar helm-mpg123-playlists
  '((name . "Play List")
    (candidates . (lambda () (directory-files *mpg123-dir* nil "\\.m3u$")))
    (action . helm-mpg123-open-playlist)))

(defun helm-mpg123 () (interactive) (helm :sources (list helm-mpg123-playlists)))
(defun helm-mpg123-open-playlist (f) (interactive) (mpg123-mode f))
```

#### For Anything
```
(defvar anything-mpg123-playlists
  '((name . "Play List")
    (candidates . (lambda () (directory-files *mpg123-dir* nil "\\.m3u$")))
    (action . (("Open PlayList" . anything-mpg123-open-playlist)))))

(defun anything-mpg123 () (interactive) (anything (list anything-mpg123-playlists)))
(defun anything-mpg123-open-playlist (f) (interactive) (mpg123-mode f))
```

