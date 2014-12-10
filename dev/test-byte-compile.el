(require 'ert)

(ert-deftest dash-byte-compile ()
  (let* ((cmd "emacs --quick --batch -f batch-byte-compile dash.el")
         (res (shell-command-to-string cmd))
         (elc (expand-file-name "dash.elc"))
         (exp (format "Wrote %s\n" elc)))
    (should (string= res exp))))
