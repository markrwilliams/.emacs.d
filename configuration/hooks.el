;;; hooks -- All hook configuration
;;;
;;; Commentary:
;;; If possible don't use this!  Configure things with use-package in
;;; packages.el
;;;
;;; Code:

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; hooks.el ends here
