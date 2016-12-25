;;; contain-customize.el -- Contain customizer
;;;
;;; Commentary:
;;; Contain M-x customize gunk in one place
;;;
;;; Code:

(setq custom-file (mrw-configuration-path "custom-file"))
(load custom-file 'noerror)

;;; contain-customize.el ends here
