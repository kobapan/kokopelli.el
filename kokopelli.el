;; kokopelli.el -*-mode: Emacs-Lisp; tab-width: 4;-*- .

;; List function declaration and jump to it.
;; Copyright (C) 2009, Kobayashi Takaaki <kobapan at gmail dot com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Version    : $Id$
;; Author     : Kobayashi Takaaki <kobapan at gmail dot com>

;; Installation
;; 
;; Add kokopelli.el to your load path 
;; add your .emacs
;;
;; (require 'kokopelli)
;; (define-key global-map "\C-ck" 'kokopelli-sing)
;;

;; Usage
;;
;; Type C-ck to list functions.
;; Type SPACE or ENTER to jump to it.
;;

;; Code
(defun kokopelli-sing ()
  "interactive to create buffer listing class and function"
  (interactive)
  (let (listing-regexp kokopelli-buffer
        (mode (downcase mode-name))
        (file-path (when (not (null buffer-file-name))
                     buffer-file-name))
        (init-kokopelli
         (lambda (file-name)
           (let ((buffer-to-back (current-buffer))
                 (kokopelli-buffer-name "*kokopelli*"))
             (save-excursion
               (if (null (buffer-live-p kokopelli-buffer-name))
                   (progn
                     (split-window nil nil t)
                     (pop-to-buffer kokopelli-buffer-name))
                 (set-buffer kokopelli-buffer-name))
               (setq buffer-read-only nil) ; unlock
               (erase-buffer)
               (insert file-name "\n")
               (setq buffer-read-only t)   ; lock
               (goto-char (point-max))
               (pop-to-buffer buffer-to-back)
               (get-buffer kokopelli-buffer-name)))))
        (insert-into-kokopelli
         (lambda (kokopelli-buffer string place)
           (let ((buffer-to-back (current-buffer))
                 start
                 end)
             (set-buffer kokopelli-buffer)
             (setq buffer-read-only nil) ; unlock
             (setq start (point))
             (insert string)
             (setq end (point))
             (insert "\n")
             (put-text-property start end 'goto place)
             (setq buffer-read-only t)   ; lock
             (set-buffer buffer-to-back))))
        (kokopelli-mode
         (lambda (kokopelli-buffer)
           (let ((map (make-sparse-keymap)))
             (pop-to-buffer kokopelli-buffer)
             (setq mode-name "kokopelli-mode")
             (define-key map [double-mouse-1] 'kokopelli-jump)
             (define-key map "\r" 'kokopelli-jump)
             (define-key map " " 'kokopelli-jump)
             (use-local-map map)))))
    (setq listing-regexp (cond ((or (equal mode "emacs-lisp")
                                     (equal mode "e-lisp")
                                     (equal mode "el")
                                     (equal mode "lisp")
                                     (equal mode "lisp interaction"))
                                "^\\([ \t]*(\\(defun\\|defvar\\|defconst\\|defconstant\\|defclass\\|defcustom\\|defgroup\\)[ \t]+\\([^ ;\t\n]*\\)\\([ ;\t\n]+.*\\)\\)$")
                               ((or (equal mode "c++")
                                    (equal mode "c")
                                    (equal mode "cpp")
                                    (equal mode "cc")
                                    (equal mode "c/l")
                                    (equal mode "c"))
                                "\\(^\\([^ \t\n#/\\*]+\\)\\([0-9a-zA-Z_ \t\\*]+\\)([^\n;]*\\)$")
                               ((or (equal mode "perl")
                                    (equal mode "cperl")
                                    (equal mode "pl"))
                                "^\\([ \t]*\\(sub\\)[ \t]+\\(.*\\)\\)$")
                               ((or (equal mode "ruby")
                                    (equal mode "rb"))
                                "^\\([ \t]*\\(class\\|module\\|def\\|alias\\)[ \t]+\\(.*\\)\\)$")
                               ((equal mode "php")
                                "^\\([ \t]*\\(public\\|private\\|protected\\)*[ \t]*\\(function\\|class\\)[ \t]+\\([^\(\{]*\\).*\\)$")
                               ((equal mode "html")
                                "^\\([ \t]*<[Hh][123456].*\\|[ \t]*<[Hh][Ee][Aa][Dd].*\\|[ \t]*<[Bb][Oo][Dd][Yy].*\\|[ \t]*<[Ff][Oo][Rr][Mm].*\\)$")
                               ((equal mode "text")
                                "^\\([ \t]*[1234567890]+[\.]+.*\\)$")
                               ((equal mode "tex")
                                "^\\(\\\\chapter.*\\|\\\\section.*\\|\\\\subsection.*\\|\\\\subsubsection.*\\)$")
                               ((or (equal mode "pascal")
                                    (equal mode "pas"))
                                "^\\([Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee].*\\)$")
                               ((or (equal mode "java")
                                    (equal mode "jav"))
                                "\\(^[ \t]*[^\n#/\\*=]+[0-9a-zA-Z_ \t\\*,\.()]+{[^\n;]*\\)$")
                               ((or (equal mode "basic")
                                    (equal mode "bas"))
                                "^\\(\\([Pp][Rr][Ii][Vv][Aa][Tt][Ee]\\|[Pp][Uu][Bb][Ll][Ii][Cc]\\|[Ss][Uu][Bb]\\|[F][U][N][C][T][I][O][N]\\)[ \t]+.*\\)$")
                               (t
                                (error "invalied mode"))))
    (setq kokopelli-buffer (funcall init-kokopelli file-path))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward listing-regexp nil t)
        (funcall insert-into-kokopelli
                 kokopelli-buffer
                 (buffer-substring (match-beginning 1) (match-end 1)) 
                 (match-beginning 1))))
    (funcall kokopelli-mode kokopelli-buffer)))

(defun kokopelli-jump ()
  "interactive function jump from kokopelli buffer to searching class or function"
  (interactive)
  (let ((buffer-to-back (current-buffer))
        aim-point
        file-name)
    (save-excursion
      (setq aim-point (get-text-property (point) 'goto))
      (goto-char (point-min))
      (setq file-name (buffer-substring (point) (progn (end-of-line) (point))))
      (pop-to-buffer (find-file-noselect file-name))
      (goto-char aim-point)
      (pop-to-buffer buffer-to-back))))
                  
(provide 'kokopelli)
;; kokopelli.el



