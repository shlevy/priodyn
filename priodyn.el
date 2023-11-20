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
;; are higher priority), etc.  This enables integrating a larger,
;; more diverse array of projects than manual prioritization and
;; scheduling on its own, and encourages integration across projects.

;; priodyn is built on top of 'org-roam'.  Projects are represented by
;; org-roam files tagged with 'priodyn-project-tag'.  You can run 'priodyn'
;; to see your project list, and call 'priodyn-manage-agenda' to have your
;; org-agenda be built based off of 'priodyn's project list.

;; Currently, priodyn orders projects by recency of modification.

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

(defcustom priodyn-extra-agenda-files nil
  "Extra files to be appended to the priodyn-generated agenda.

See documentation for variable `org-agenda-files'."
  :group 'priodyn
  :type '(repeat :tag "List of files and directories" file))

(defun priodyn-projects ()
  "Calculate the priodyn project list."
  (let ((projects (org-roam-db-query
		   [:select [nodes:title nodes:file files:mtime]
		    :from tags
		    :left-join nodes
		    :on (= tags:node-id nodes:id)
		    :left-join files
		    :on (= nodes:file files:file)
		    :where (= tag $s1)
		    ]
		   priodyn-project-tag)))
    (sort projects (lambda (e1 e2)
		     (let ((m1 (nth 2 e1))
			   (m2 (nth 2 e2)))
		       (time-less-p m2 m1))))))

(defun priodyn--set-agenda-files ()
  "Set variable `org-agenda-files' from priodyn's project list.

See also `priodyn-extra-agenda-files'."
  (let* ((projects (priodyn-projects))
	 (project-files (mapcar (lambda (proj) (pcase-let ((`(,title ,file) proj)) file)) projects)))
    (customize-set-variable 'org-agenda-files (append project-files priodyn-extra-agenda-files))))

;;;###autoload
(defun priodyn-manage-agenda ()
  "Use priodyn's project list to manage the agenda.

The agenda is populated from priodyn projects in priority order.

See also `priodyn-extra-agenda-files'."
  (interactive)
  (add-hook 'org-agenda-mode-hook 'priodyn--set-agenda-files))

(defun priodyn-unmanage-agenda ()
  "Stop using priodyn to manage the agenda.

You will have to manually restore the agenda list if needed."
  (interactive)
  (remove-hook 'agenda-mode-hook 'priodyn--set-agenda-files))

;;;###autoload
(defun priodyn ()
  "Display the priodyn project buffer."
  (interactive)
  (let ((projects (priodyn-projects)))
    (with-current-buffer
	(get-buffer-create "*priodyn*")
      (erase-buffer)
      (dolist (proj projects)
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
