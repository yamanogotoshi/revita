;;; revita.el --- daily task management helper library  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ryoto Yamashita

;; Author: Ryoto Yamashita <yamasita.ryoto@gmail.com>
;; URL: https://github.com/yamanogotoshi/revita.el
;; Version: 0.0.1
;; Package-Requires: 
;; Keywords: task management
;; License: GPL-3+

;;; Commentary:
;;
;; This is an Emacs Lisp library that provides helper functions
;; for daily task management.

;;; Code:

(require 'org)

(defgroup revita-project nil
  "Settings for revita project management."
  :group 'tools
  :prefix "revita-")

(defcustom revita-project-directory
  (expand-file-name (concat "~/work/" (format-time-string "%Y")))
  "Base directory in which to store project files.
For example, \"~/work/2025\". Each project will be placed under this directory."
  :type 'directory
  :group 'revita-project)

(defcustom revita-default-org-content "* memo\n\n* log\n\n* task\n"
  "Default content inserted into a newly created project Org file."
  :type 'string
  :group 'revita-project)

(defcustom revita-project-file-alist '()
  "Mapping of project tag and file path."
  :type '(alist :key-type string :value-type string)
  :group 'revita-project)

(defun revita-save-project-file-alist ()
  "Save revita-project-file-alist using custom-set-variables."
  (customize-save-variable 'revita-project-file-alist revita-project-file-alist))

(defun revita-insert-project-link ()
  "Insert a link to the file corresponding to the current task's tag."
  (interactive)
  (let* ((tags (org-get-tags))
         (proj-tag (if (= (length tags) 1)
                       (car tags)
                     (completing-read "Select project tag: " tags nil t)))
	 (default-path (expand-file-name
                        (format "%s.org" proj-tag)
                        (expand-file-name proj-tag revita-project-directory)))
         (file-path (or (cdr (assoc proj-tag revita-project-file-alist))
                        (read-file-name
                         (format "Enter file path for project %s (default: %s): "
                                 proj-tag default-path)
                         (file-name-directory default-path)
                         nil
                         nil
                         (file-name-nondirectory default-path)))))
    (if proj-tag
        (progn
          (unless (file-exists-p file-path)
            (when (y-or-n-p (format "File %s does not exist. Create it? " file-path))
              (make-directory (file-name-directory file-path) t)
              (with-temp-buffer
		(insert revita-default-org-content)
                (write-file file-path))))
          (setq revita-project-file-alist
                (cons (cons proj-tag file-path)
                      (assoc-delete-all proj-tag revita-project-file-alist)))
          (customize-save-variable 'revita-project-file-alist revita-project-file-alist)
          (org-end-of-subtree)
          (insert "\n")
          (org-insert-link nil file-path proj-tag))
      (message "Tag not found"))))

(defun revita-change-project-file-path (tag)
  "Change the project file path for the specified tag."
  (interactive "sEnter tag name: ")
  (let* ((current-path (cdr (assoc tag revita-project-file-alist)))
         (new-path (read-file-name
                    (format "Enter new file path (current: %s): " current-path)
                    (file-name-directory current-path))))
    (setq revita-project-file-alist
          (cons (cons tag new-path)
                (assoc-delete-all tag revita-project-file-alist)))
    (message "Updated path for tag '%s' to '%s'" tag new-path)))

(defun revita-open-project-file ()
  "Select a tag from the current task or revita-project-file-alist
and open the corresponding project file.
If the file is not registered, prompt for the path.
If the file does not exist, create the project file."
  (interactive)
  ;; Get tags from the current heading only if in org-mode buffer
  (let* ((tags (when (derived-mode-p 'org-mode) (org-get-tags)))
         (proj-tag
          (cond
           ;; If there is only one tag, use it directly
           ((= (length tags) 1)
            (car tags))
           ;; If there are multiple tags, let the user choose
           ((> (length tags) 1)
            (completing-read "Select project tag: " tags nil t))
           ;; If no tags / outside Org heading, choose from revita-project-file-alist
           (t
            (completing-read
             "Select project tag (registered tags): "
             (mapcar #'car revita-project-file-alist)
             nil t))))
         ;; Construct the default path
         (default-path (expand-file-name
                        (format "%s.org" proj-tag)
                        (expand-file-name proj-tag revita-project-directory)))
         ;; Get the file path from the alist
         (file-path (cdr (assoc proj-tag revita-project-file-alist))))
    (if proj-tag
        (progn
          ;; If the tag is not registered, prompt the user for input
          (unless file-path
            (setq file-path
                  (read-file-name
                   (format "Enter file path for project %s (default: %s): "
                           proj-tag default-path)
                   (file-name-directory default-path)
                   nil
                   nil
                   (file-name-nondirectory default-path)))
            (setq revita-project-file-alist
                  (cons (cons proj-tag file-path)
                        (assoc-delete-all proj-tag revita-project-file-alist)))
            (customize-save-variable 'revita-project-file-alist revita-project-file-alist))
          ;; If it doesn't exist, create a new one
          (unless (file-exists-p file-path)
            (when (y-or-n-p (format "File %s does not exist. Create it? " file-path))
              (make-directory (file-name-directory file-path) t)
              (with-temp-buffer
                (insert revita-default-org-content)
                (write-file file-path))))
          ;; Finally, open the file
          (find-file file-path))
      (message "Tag not found"))))

(defun revita-org-extract-clock-times ()
  "Extract start/end times from CLOCK lines in the current entry's LOGBOOK and set them as properties.
   Do not change existing :START_TIME:, overwrite :END_TIME:."
  (save-excursion
    (let (start-time end-time)
      (when (re-search-forward org-clock-string nil t)
        (let ((clock-line (thing-at-point 'line t)))
          (when (string-match "\\[\\([0-9]+:[0-9]+\\)\\].*--\\[\\([0-9]+:[0-9]+\\)\\]" clock-line)
            (setq start-time (match-string 1 clock-line))
            (setq end-time (match-string 2 clock-line)))))
      ;; Do not change :START_TIME: if it already exists
      (unless (org-entry-get (point) "START_TIME")
        (when start-time (org-entry-put (point) "START_TIME" start-time)))
      ;; Always overwrite :END_TIME:
      (when end-time (org-entry-put (point) "END_TIME" end-time)))))

(defun revita-org-update-start-time ()
  "Update :START_TIME: on CLOCK IN. However, do not change if :START_TIME: already exists."
  (save-excursion
    (org-back-to-heading t)
    (unless (org-entry-get (point) "START_TIME") ;; Do not update if it already exists
      (let ((start-time (format-time-string "%H:%M")))
        (org-entry-put (point) "START_TIME" start-time)))))

(defun revita-org-update-end-time ()
  "Update :END_TIME: on CLOCK OUT, and supplement :START_TIME: if necessary.
   However, do not change if :START_TIME: already exists."
  (save-excursion
    (org-back-to-heading t)
    (let ((end-time (format-time-string "%H:%M")))
      (org-entry-put (point) "END_TIME" end-time) ;; Always overwrite
      (unless (org-entry-get (point) "START_TIME")
        (revita-org-extract-clock-times)))))

(defun revita-org-add-logbook-if-missing ()
  "Add a LOGBOOK drawer to the current entry if it is missing.
   If a PROPERTIES drawer already exists, add it immediately after."
  (save-excursion
    (org-back-to-heading t)
    (unless (re-search-forward "^[ \t]*:LOGBOOK:" (save-excursion (org-end-of-subtree t)) t)
      (let ((logbook-pos (save-excursion
                           (when (re-search-forward "^[ \t]*:PROPERTIES:" (save-excursion (org-end-of-subtree t)) t)
                             (re-search-forward "^[ \t]*:END:" nil t)
                             (point)))))
        (if logbook-pos
            (goto-char logbook-pos) ;; Move to the last line of PROPERTIES
          (end-of-line)) ;; If not found, add to the end as usual
        (insert "\n:LOGBOOK:\n:END:\n")))))

(provide 'revita)
