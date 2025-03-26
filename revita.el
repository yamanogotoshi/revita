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
  "現在のタスクのタグに対応するファイルへのリンクを挿入する"
  (interactive)
  (let* ((tags (org-get-tags))
         (proj-tag (if (= (length tags) 1)
                       (car tags)
                     (completing-read "プロジェクトタグを選択: " tags nil t)))
	 (default-path (expand-file-name
                        (format "%s.org" proj-tag)
                        (expand-file-name proj-tag revita-project-directory)))
         (file-path (or (cdr (assoc proj-tag revita-project-file-alist))
                        (read-file-name 
                         (format "プロジェクト %s のファイルパスを入力 (デフォルト: %s): " 
                                 proj-tag default-path)
                         (file-name-directory default-path)
                         nil
                         nil
                         (file-name-nondirectory default-path)))))
    (if proj-tag
        (progn
          (unless (file-exists-p file-path)
            (when (y-or-n-p (format "ファイル %s が存在しません。作成しますか？ " file-path))
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
      (message "タグが見つかりません"))))

(defun revita-change-project-file-path (tag)
  "指定されたタグのプロジェクトファイルパスを変更する"
  (interactive "sタグ名を入力: ")
  (let* ((current-path (cdr (assoc tag revita-project-file-alist)))
         (new-path (read-file-name 
                    (format "新しいファイルパスを入力 (現在: %s): " current-path)
                    (file-name-directory current-path))))
    (setq revita-project-file-alist
          (cons (cons tag new-path)
                (assoc-delete-all tag revita-project-file-alist)))
    (message "タグ '%s' のパスを '%s' に更新しました" tag new-path)))

(defun revita-open-project-file ()
  "現在のタスクのタグ、または revita-project-file-alist のタグを選んで
対応するプロジェクトファイルを開く。
ファイルが登録されていない場合はパスを入力し、
存在しない場合はプロジェクトファイルを作成する。"
  (interactive)
  ;; 現在の heading からタグを取得
  (let* ((tags (org-get-tags))
         (proj-tag
          (cond
           ;; タグが1つだけならそのまま使う
           ((= (length tags) 1)
            (car tags))
           ;; タグが複数あればユーザーに選んでもらう
           ((> (length tags) 1)
            (completing-read "プロジェクトタグを選択: " tags nil t))
           ;; タグが無い/Org heading外の場合は revita-project-file-alist から選ぶ
           (t
            (completing-read
             "プロジェクトタグを選択 (登録済みタグ): "
             (mapcar #'car revita-project-file-alist)
             nil t))))
         ;; デフォルトパスを組み立てる
         (default-path (expand-file-name
                        (format "%s.org" proj-tag)
                        (expand-file-name proj-tag revita-project-directory)))
         ;; alist からファイルパスを取得
         (file-path (cdr (assoc proj-tag revita-project-file-alist))))
    (if proj-tag
        (progn
          ;; 未登録タグならユーザーに入力を促す
          (unless file-path
            (setq file-path
                  (read-file-name
                   (format "プロジェクト %s のファイルパスを入力 (デフォルト: %s): "
                           proj-tag default-path)
                   (file-name-directory default-path)
                   nil
                   nil
                   (file-name-nondirectory default-path)))
            (setq revita-project-file-alist
                  (cons (cons proj-tag file-path)
                        (assoc-delete-all proj-tag revita-project-file-alist)))
            (customize-save-variable 'revita-project-file-alist revita-project-file-alist))
          ;; 実在しない場合は新規作成
          (unless (file-exists-p file-path)
            (when (y-or-n-p (format "ファイル %s が存在しません。作成しますか？ " file-path))
              (make-directory (file-name-directory file-path) t)
              (with-temp-buffer
                (insert revita-default-org-content)
                (write-file file-path))))
          ;; 最後にファイルを開く
          (find-file file-path))
      (message "タグが見つかりません"))))

(defun revita-org-extract-clock-times ()
  "現在のエントリーの LOGBOOK の CLOCK 行から開始・終了時刻を取得し、プロパティに設定する。
   既存の :START_TIME: は変更せず、:END_TIME: は上書きする。"
  (save-excursion
    (let (start-time end-time)
      (when (re-search-forward org-clock-string nil t)
        (let ((clock-line (thing-at-point 'line t)))
          (when (string-match "\\[\\([0-9]+:[0-9]+\\)\\].*--\\[\\([0-9]+:[0-9]+\\)\\]" clock-line)
            (setq start-time (match-string 1 clock-line))
            (setq end-time (match-string 2 clock-line)))))
      ;; :START_TIME: は既に存在する場合は変更しない
      (unless (org-entry-get (point) "START_TIME")
        (when start-time (org-entry-put (point) "START_TIME" start-time)))
      ;; :END_TIME: は常に上書き
      (when end-time (org-entry-put (point) "END_TIME" end-time)))))

(defun revita-org-update-start-time ()
  "CLOCK IN 時に :START_TIME: を更新。ただし、既に :START_TIME: がある場合は変更しない。"
  (save-excursion
    (org-back-to-heading t)
    (unless (org-entry-get (point) "START_TIME") ;; 既にあれば更新しない
      (let ((start-time (format-time-string "%H:%M")))
        (org-entry-put (point) "START_TIME" start-time)))))

(defun revita-org-update-end-time ()
  "CLOCK OUT 時に :END_TIME: を更新し、必要なら :START_TIME: も補完。
   ただし、:START_TIME: が既に存在する場合は変更しない。"
  (save-excursion
    (org-back-to-heading t)
    (let ((end-time (format-time-string "%H:%M")))
      (org-entry-put (point) "END_TIME" end-time) ;; 常に上書き
      (unless (org-entry-get (point) "START_TIME")
        (revita-org-extract-clock-times)))))

(defun revita-org-add-logbook-if-missing ()
  "現在のエントリーに LOGBOOK ドロワーがない場合、追加する。
   すでに PROPERTIES ドロワーがある場合、その直後に追加する。"
  (save-excursion
    (org-back-to-heading t)
    (unless (re-search-forward "^[ \t]*:LOGBOOK:" (save-excursion (org-end-of-subtree t)) t)
      (let ((logbook-pos (save-excursion
                           (when (re-search-forward "^[ \t]*:PROPERTIES:" (save-excursion (org-end-of-subtree t)) t)
                             (re-search-forward "^[ \t]*:END:" nil t)
                             (point)))))
        (if logbook-pos
            (goto-char logbook-pos) ;; PROPERTIES の最終行に移動
          (end-of-line)) ;; なければ通常通り末尾に追加
        (insert "\n:LOGBOOK:\n:END:\n")))))

(provide 'revita)
