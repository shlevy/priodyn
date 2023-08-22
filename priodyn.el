;;; priodyn.el --- Project management with dynamic prioritization

;; Copyright 2023 Shea Levy

;; Author: Shea Levy
;; URL: https://github.com/shlevy/priodyn/
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.2") (org-roam "2.2.2"))

;;; Commentary:

;; This package implements a project management system where
;; project priorities dynamically reflect information such as
;; recency (projects one has more recently worked on have
;; higher priority), connectivity (projects connected to more
;; or more important issues in your knowledge management system
;; are higher priority), etc. This enables integrating a larger,
;; more diverse array of projects than manual prioritization and
;; scheduling on its own, and encourages integration across projects.

;;; Code:

(require 'button)
(require 'org-roam)

(defgroup priodyn nil "Customization for priodyn."
  :prefix "priodyn-"
  :group 'org
  :package-version '('priodyn . "1.0.0"))

(defcustom priodyn-project-tag "project"
  "Tag to identify projects within priodyn."
  :group 'priodyn
  :type '(string))

;;;###autoload
(defun priodyn ()
  "Load the priodyn project list."
  (interactive)
  (let* ((projects (org-roam-db-query
		    [:select [nodes:title nodes:file files:mtime]
		     :from tags
		     :left-join nodes
		     :on (= tags:node-id nodes:id)
		     :left-join files
		     :on (= nodes:file files:file)
		     :where (= tag $s1)
		     ]
		    priodyn-project-tag))
	 (sorted-projects (sort projects (lambda (e1 e2)
					   (let ((m1 (nth 2 e1))
						 (m2 (nth 2 e2)))
					     (time-less-p m2 m1))))))
    (with-current-buffer
	(get-buffer-create "*priodyn*")
      (erase-buffer)
      (dolist (proj sorted-projects)
	(pcase-let ((`(,title ,file) proj))
	  (insert-button title
			 'action (lambda (x) (find-file-other-window (button-get x 'file)))
			 'file file))
	(insert "\n"))
      (pop-to-buffer (current-buffer)))))

(provide 'priodyn)

;;; priodyn.el ends here

;; Local Variables:
;; tab-width: 8
;; End:
