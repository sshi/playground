(cd "~/")

(require 'un-define)
(require 'jisx0213)
(set-language-environment "Japanese")
;(prefer-coding-system 'euc-jp)
;(set-keyboard-coding-system 'euc-jp)

;;; IME setting
(defun ime-setting()
  (mw32-ime-initialize)
  (setq default-input-method "MW32-IME")
  (setq-default mw32-ime-mode-line-state-indicator "[--]")
  (setq mw32-ime-mode-line-state-indicator-list '("[--]" "[SKK]" "[--]"))
  (add-hook 'mw32-ime-on-hook
	    (function (lambda () (set-cursor-height 2))))
  (add-hook 'mw32-ime-off-hook
	    (function (lambda () (set-cursor-height 4)))))

(defun font-setting-xp ()
  (let ((make-spec 
	 (function 
 	  (lambda (size charset fontname &optional windows-charset)
 	    (setq size (- size))
 	    (if (not windows-charset)
 		(setq windows-charset 
 		      (cadr 
 		       (assq charset mw32-charset-windows-font-info-alist))))
 	    `(((:char-spec ,charset :height any)
 	       strict
 	       (w32-logfont ,fontname 0 ,size 400 0 nil nil nil ,
 			    windows-charset 1 3 0))
 	      ((:char-spec ,charset :height any :weight bold)
 	       strict
 	       (w32-logfont ,fontname 0 ,size 700 0 nil nil nil ,
 			    windows-charset 1 3 0)
 	       ((spacing . -1)))
 	      ((:char-spec ,charset :height any :slant italic)
 	       strict
 	       (w32-logfont ,fontname 0 ,size 400 0   t nil nil ,
 			    windows-charset 1 3 0))
 	      ((:char-spec ,charset :height any :weight bold :slant italic)
 	       strict
 	       (w32-logfont ,fontname 0 ,size 700 0   t nil nil ,
 			    windows-charset 1 3 0)
 	       ((spacing . -1)))))))
	
	(make-spec-list
	 (function
	  (lambda (size fontset-list)
	    (list (cons 'spec 
			(apply 'append 
			       (mapcar (lambda (fontset)
					 (apply make-spec (cons size fontset)))
				       fontset-list)))))))
	
	(define-fontset 
	  (function
	   (lambda (fontname size fontset-list)
	     (let ((spec (funcall make-spec-list size fontset-list)))
	       (if (w32-list-fonts fontname)
		   (w32-change-font fontname spec)
		 (w32-add-font fontname spec))))))
	
	(serif-fontset-list
	 '((ascii "MS Mincho")
	   (katakana-jisx0201 "MS Mincho")
	   (japanese-jisx0208 "MS Mincho")
	   ;(ascii "IPA明朝")
	   ;(katakana-jisx0201 "IPA明朝")
	   ;(japanese-jisx0208 "IPA明朝")
	   (korean-ksc5601 "Batang")
	   (chinese-gb2312 "SimSun")
	   (chinese-big5-1 "MingLiU")
	   (chinese-big5-2 "MingLiU")))

	(sans-serif-fontset-list
	 '((ascii "MS Gothic")
	   (katakana-jisx0201 "MS Gothic")
	   (japanese-jisx0208 "MS Gothic")
	   ;(ascii "IPAゴシック")
	   ;(katakana-jisx0201 "IPAゴシック")
	   ;(japanese-jisx0208 "IPAゴシック")

	   (korean-ksc5601 "Dotum")
	   (chinese-gb2312 "SimHei")
	   (chinese-big5-1 "MingLiU")
	   (chinese-big5-2 "MingLiU"))))
	
  (funcall define-fontset "Serif 10" 10 serif-fontset-list)
  (funcall define-fontset "Serif 12" 12 serif-fontset-list)
  (funcall define-fontset "Serif 14" 14 serif-fontset-list)
  (funcall define-fontset "Serif 16" 16 serif-fontset-list)
  (funcall define-fontset "Serif 18" 18 serif-fontset-list)
  (funcall define-fontset "Serif 20" 20 serif-fontset-list)
  (funcall define-fontset "Serif 22" 22 serif-fontset-list)
  (funcall define-fontset "Serif 24" 24 serif-fontset-list)
  (funcall define-fontset "Serif 36" 36 serif-fontset-list)
  (funcall define-fontset "Serif 48" 48 serif-fontset-list)
  (funcall define-fontset "Sans Serif 10" 10 sans-serif-fontset-list)
  (funcall define-fontset "Sans Serif 12" 12 sans-serif-fontset-list)
  (funcall define-fontset "Sans Serif 14" 14 sans-serif-fontset-list)
  (funcall define-fontset "Sans Serif 16" 16 sans-serif-fontset-list)
  (funcall define-fontset "Sans Serif 18" 18 sans-serif-fontset-list)
  (funcall define-fontset "Sans Serif 20" 20 sans-serif-fontset-list)
  (funcall define-fontset "Sans Serif 22" 22 sans-serif-fontset-list)
  (funcall define-fontset "Sans Serif 24" 24 sans-serif-fontset-list)
  (funcall define-fontset "Sans Serif 36" 36 sans-serif-fontset-list)
  (funcall define-fontset "Sans Serif 48" 48 sans-serif-fontset-list))
  
  (setq default-frame-alist 
	(cons '(font . "Sans Serif 12") default-frame-alist)))


;; key-map change 
(defun key-map-setting ()
  (define-key global-map "\C-\\" 'undo)
  (define-key global-map "\C-h" 'delete-backward-char))


;for scala
(defun scala-setting()
  (setq load-path (cons "~/lib/elisp/scala" load-path))
  (require 'scala-mode-auto)
  (setq scala-interpreter "C:/bin/scala/bin/scala.bat"))

;for haskell
(defun haskell-setting()
  (setq load-path (cons "~/lib/elisp/haskell" load-path))
  (setq auto-mode-alist
	(append auto-mode-alist
		'(("\\.[hg]s$"  . haskell-mode)
		  ("\\.hi$"     . haskell-mode)
		  ("\\.l[hg]s$" . literate-haskell-mode))))
  (autoload 'haskell-mode "haskell-mode"
    "Major mode for editing Haskell scripts." t)
  (autoload 'literate-haskell-mode "haskell-mode"
    "Major mode for editing literate Haskell scripts." t)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
  
  (setq haskell-literate-default 'latex)
  (setq haskell-doc-idle-delay 0)
  
  (setq haskell-ghci-program-args '("-XArrows")))
	

;; for ruby
(defun ruby-setting ()
  (setq load-path (cons "~/lib/elisp/ruby" load-path))
  (autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
  (setq auto-mode-alist (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
  (setq interpreter-mode-alist 
	(append '(("ruby" . ruby-mode)) interpreter-mode-alist))
  
  (autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
  (autoload 'inf-ruby-keys "inf-ruby" 
    "Set local key defs for inf-ruby in ruby-mode")
  (add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))
  (setq ruby-program-name "C:/PROGRA~1/ruby-1.9.1/bin/irb.bat --inf-ruby-mode"))
	    

;; for python
(defun python-setting ()
  (setq load-path (cons "~/lib/elisp/python" load-path))
  (autoload 'python-mode "python-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode)))
  ;(add-hook 'python-mode-hook
  ;'(lambda() (require 'pycomplete) (setq indent-tabs-mode nil))))

(setq default-frame-alist
      (append (list '(foreground-color . "white")
		    '(background-color . "black") 
;		    '(background-color . "LemonChiffon")
;		    '(background-color . "Alice blue")
;		    '(background-color . "gray")
		    '(border-color . "black")
		    '(mouse-color . "white")
		    '(cursor-color . "black")
		    '(width . 80)
		    '(height . 70)
;		    '(top . 100)
;		    '(left . 100)
		    )
	      default-frame-alist))

;; howm
(defun howm-setting ()
  (add-to-list 'load-path "~/lib/elisp/howm/")
  (setq howm-menu-lang 'ja)
  (require 'howm))

;; psvn
(defun psvn-setting()
  (require 'psvn)
)

(defun auto-save-setting ()
  (require 'auto-save-buffers)
  (run-with-idle-timer 0.5 t 'auto-save-buffers) )

(defun tuareg-mode-setting ()
  (add-to-list 'load-path "~/lib/elisp/tuareg/")
  (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t))

(defun scheme-setting ()
  (setq scheme-program-name "C:/bin/larceny/larceny.bat"))

(defun slime-setting()
  (add-to-list 'load-path "~/lib/elisp/slime/")
  (setq inferior-lisp-program "sbcl")
  (require 'slime)
  (slime-setup))

;; my setting
(line-number-mode 1)
(column-number-mode 1)
(font-setting-xp)

(ime-setting)

(toggle-input-method nil)
(setq visible-bell t)
(global-font-lock-mode t)
(scheme-setting)
(scala-setting)

(key-map-setting)
(howm-setting)
(psvn-setting)
(haskell-setting)
;(auto-save-setting)
(ruby-setting)
(tuareg-mode-setting)
(python-setting)
(slime-setting)
