;;; tls-trust.el -- Make TLS trust make sense.
;;;
;;; Commentary:

;;; Ensure that gnutls, even when linked in, verifies certificates,
;;; and emit a message listing the CA files.  Thanks, glyph!
;;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html
;;;
;;; Code:

(setq gnutls-verify-error t)

(add-hook 'after-init-hook (lambda ()
                             (require 'gnutls)
                             (message "TLS trust stores: %s" gnutls-trustfiles)))


;;; tls-trust.el ends here
