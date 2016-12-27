;;; switch-panel -- Switch emacs features on or off emacs
;;;
;;; Commentary:
;;; Boolean settings only!
;;;
;;; Code:

;; Prefer X's selection buffer for copying and pasting...
(setq select-enable-clipboard nil)      ;...don't use the X clipboard

(setq select-enable-primary t)          ;...instead use the primary
                                        ;buffer (middle click!)

(setq mouse-drag-copy-region t)         ;...selecting a region places
                                        ;it in the kill buffer
;; save history!
(savehist-mode 1)
;; can't set this in customize-variable because it wants an integer,
;; even though its documentation says that t means save everything
(setq history-length t)
(savehist-mode 1)

;; handle compressed files
(auto-compression-mode 1)

;; no scroll bar
(scroll-bar-mode -1)
;; no tool bar
(tool-bar-mode -1)
;; no menu bar
(menu-bar-mode -1)

;; no backup files
(setq make-backup-files nil)

;; server!
(server-start)

;; find file at point
(ffap-bindings)

;; interactively do things!  what about helm?
(ido-mode)

;; make duplicate file names unique
(require 'uniquify)
;; like/this or/this
(setq uniquify-buffer-name-style 'forward)

;; shorten things up
(defalias 'yes-or-no-p 'y-or-n-p)

;; don't tell me what to do
(setq disabled-command-function nil)

;; nice size
(set-face-attribute 'default nil :height 100)

;; () [] {} '' ""
(electric-pair-mode)

;;; switch-panel.el ends here
