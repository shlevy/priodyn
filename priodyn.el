;;; priodyn.el --- Project management with dynamic prioritization

;; Copyright 2023 Shea Levy

;; Author: Shea Levy
;; URL: https://github.com/shlevy/priodyn/
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.2"))

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

(provide 'priodyn)

;;; priodyn.el ends here

;; Local Variables:
;; tab-width: 8
;; End:
