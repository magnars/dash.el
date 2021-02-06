;;; dash-functional.el --- Collection of useful combinators for Emacs Lisp  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2021 Free Software Foundation, Inc.

;; Author: Matus Goljer <matus.goljer@gmail.com>
;;         Magnar Sveen <magnars@gmail.com>
;; Version: 1.3.0
;; Package-Requires: ((dash "2.18.0"))
;; Keywords: extensions, lisp
;; Homepage: https://github.com/magnars/dash.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; *N.B.:* This package has been absorbed, and is therefore made
;; obsolete, by the `dash' package, version 2.18.0.
;;
;; If you maintain a package that depends on `dash-functional', then
;; you should change that to instead depend on `dash' version 2.18.0,
;; and remove all references to `dash-functional'.
;;
;; If you use any packages that depend on `dash-functional', either
;; directly or indirectly, then you will have to wait until all of
;; them have transitioned away from it before you can remove it.
;;
;; For more information on this, see the following URL:
;; `https://github.com/magnars/dash.el/wiki/Obsoletion-of-dash-functional.el'

;;; Code:

(require 'dash)

(eval-and-compile
  (let ((msg "Package dash-functional is obsolete; use dash 2.18.0 instead"))
    (if (and noninteractive (fboundp 'byte-compile-warn))
        (byte-compile-warn msg)
      (message "%s" msg))))

(provide 'dash-functional)

;;; dash-functional.el ends here
