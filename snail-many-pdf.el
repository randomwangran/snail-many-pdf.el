;;; snails-backend-current-buffer.el --- Current buffer for snails

;; Filename: snails-backend-current-buffer.el
;; Description: Ripgrep backend for snails
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-07-23 16:41:05
;; Version: 0.1
;; Last-Updated: 2019-07-23 16:41:05
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-current-buffer.el
;; Keywords:
;; Compatibility: GNU Emacs 26.2
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Ripgrep backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-current-buffer.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-rg)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-rg RET
;;

;;; Change log:
;;
;; 2019/07/23
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;  possible add the name of the book to the list?
;;

;;; Require
(require 'snails-core)

;;; Code:


(setq snail-many-pdf-directory "YOUR_PATH")

(snails-create-async-backend
 :name
 "searchablepdf"

 :build-command
 (lambda (input)
     (when (and (executable-find "rga")
                (> (length input) 2))
       (list "rga" "--no-heading" "--column" "--color" "never" "--max-columns" "300" input (expand-file-name snail-many-pdf-directory))
       ))

 :candidate-filter
 (lambda (candidate-list)
   (let (candidates)
     (dolist (candidate candidate-list)
       (let ((candidate-info (split-string candidate ":")))
         (snails-add-candiate
          'candidates
          (format "%s: %s"
                  (snails-format-line-number (nth 0 candidate-info) snails-start-buffer-lines)
                  (string-join (cddr candidate-info)))
          candidate)))
     candidates))

 :candiate-do
 (lambda (candidate)
         (let ((file-info (split-string candidate ":")))
           (message "%s" file-info)
           (message "%s" (nth 0 file-info))
           (message "%s" (nth 1 file-info))
           (message "%s" (concat (nth 0 file-info) ":" (nth 1 file-info)))
           (message "page is: %s" (nth 3 file-info))
           (when (> (length file-info) 2)
             ;; Open file and jump to position.

             (if (get-buffer (concat (nth 0 file-info) ":" (nth 1 file-info))) (kill-buffer (concat (nth 0 file-info) ":" (nth 1 file-info))))

             (org-pdfview-open (concat (nth 0 file-info) ":" (nth 1 file-info)))

             (pdf-view-goto-page (string-to-number (substring (nth 3 file-info) 5)))


             (snails-flash-line)

             ))))


(provide 'snails-backend-searchablepdf)

;;; snails-backend-searchablepdf.el ends here
