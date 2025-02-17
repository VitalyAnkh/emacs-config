#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
(setq start-time (float-time)
      exit-code 0)

(defvar script-root default-directory)
(defvar config-root (file-name-directory ; $DOOM_DIR/
                     (directory-file-name
                      (file-name-directory ; $DOOM_DIR/misc
                       (directory-file-name
                        (file-name-directory load-file-name))))))

(defvar log-file "unnamed-log.txt")

(write-region "" nil log-file)

(setq gc-cons-threshold 16777216
          gcmh-high-cons-threshold 16777216)
    (load (expand-file-name "core/core.el" user-emacs-directory) nil t)
    (require 'core-cli)
    (doom-initialize)

(defcli! check-updates ()

  ;;; Generation of status badge

  (defun gen-status-img (upgradeable total)
    (let ((colour (pcase (/ (float upgradeable) total)
                    ((pred (= 0.0)) "brightgreen")
                    ((pred (> 0.15)) "green")
                    ((pred (> 0.25)) "yellowgreen")
                    ((pred (> 0.4)) "yellow")
                    ((pred (> 0.8)) "orange")
                    (_ "red")))
          (text (if (= 0 upgradeable)
                    (format "all %s up to date" total)
                  (format "%s/%s up to date" (- total upgradeable) total))))
      (call-process
       "wget"
       nil nil nil
       "-O"
       (expand-file-name "misc/pkg-status.svg" config-root)
       (format "https://img.shields.io/badge/packages-%s-%s.svg?style=flat-square&logo=blueprint"
               (url-hexify-string text) colour))
      (publish "misc/pkg-status.svg")))

  ;;; Show commits

  (setq upgradeable-packages 0)

  (defadvice! doom/bump-package-at-point-more-detail (&optional select)
    "Include commit messages."
    :override #'doom/bump-package-at-point
    (doom-initialize-packages)
    (cl-destructuring-bind (&key package plist beg end)
        (or (doom--package-at-point)
            (user-error "Not on a `package!' call"))
      (when (or (memq package doom-disabled-packages)
                (looking-back "^[ 	]*;.*" (line-beginning-position)))
        (user-error "Package %s is disabled, skipping." package))
      (let* ((recipe (doom--package-merge-recipes package plist))
             (branch (plist-get recipe :branch))
             (oldid (or (plist-get plist :pin)
                        (doom-package-get package :pin)))
             (url (straight-vc-git--destructure recipe (upstream-repo upstream-host)
                    (straight-vc-git--encode-url upstream-repo upstream-host)))
             (id (or (when url
                       (cdr (doom-call-process
                             "git" "ls-remote" url
                             (unless select branch))))
                     (user-error "Couldn't find a recipe for %s" package)))
             (id (car (split-string
                       (if select
                           (completing-read "Commit: " (split-string id "\n" t))
                         id))))
             (commits (unless (and oldid
                                   (plist-member plist :pin)
                                   (equal oldid id))
                        (let ((default-directory
                                (or (when (plist-member recipe :local-repo)
                                      (expand-file-name (plist-get recipe :local-repo) doom-private-dir))
                                    (straight--repos-dir
                                     (file-name-sans-extension
                                      (file-name-nondirectory url))))))
                          (doom-call-process "git" "fetch")
                          (concat "  "
                                  (cdr
                                   (doom-call-process
                                    "git" "log" "--pretty=format:  %h %s"
                                    (format "%s...%s" oldid id))))))))
        (when (and oldid (equal oldid id))
          (user-error "%s: no update necessary" package))
        (save-excursion
          (if (re-search-forward ":pin +\"\\([^\"]+\\)\"" end t)
              (replace-match id t t nil 1)
            (goto-char (1- end))
            (insert " :pin " (prin1-to-string id))))
        (cond ((not oldid)
               (message "%s: → %s" package (substring id 0 10)))
              ((< (length oldid) (length id))
               (message "%s: extended to %s..." package id))
              (t
               (setq upgradeable-packages (1+ upgradeable-packages))
               (message "%s: %s → %s\n%s"
                        package
                        (substring oldid 0 10)
                        (substring id 0 10)
                        commits))))))

  ;;; Do it

  (message "[34] Opening package file")

  (setq packages nil)
  (defun doom/bump-packages-in-buffer (&optional select)
    "Inserts or updates a `:pin' for the `package!' statement at point.
Grabs the latest commit id of the package using 'git'."
    (interactive "P")
    (save-excursion
      (goto-char (point-min))
      (doom-initialize-packages)
      (while (search-forward "(package! " nil t)
        (unless (let ((ppss (syntax-ppss)))
                  (or (nth 4 ppss)
                      (nth 3 ppss)
                      (save-excursion
                        (and (goto-char (match-beginning 0))
                             (not (plist-member (sexp-at-point) :pin))))))
          (condition-case e
              (push (doom/bump-package-at-point) packages)
            (user-error (message "%s" (error-message-string e))))
          (message "\npackage: %S\n" packages)))
      (if packages
          (message "Updated %d packages\n- %s" (length packages) (string-join packages "\n- "))
        (message "No packages to update"))))

  (with-temp-buffer
    (insert-file-contents (expand-file-name "packages.el" config-root))
    (message "[34] Checking for updates")
    (doom/bump-packages-in-buffer)
    (pp packages)
    (setq package-upgrades "")
    (goto-char (point-min))
    (setq total-packages 0)
    (while (search-forward "(package!" nil t)
      (setq total-packages (1+ total-packages))))

  (message "[32] %s total packages" total-packages)
  (message "[33] %s packages upgradable" upgradeable-packages)

  (gen-status-img upgradeable-packages total-packages)

  (when (> upgradeable-packages 0)
    (write-region
     (replace-regexp-in-string
      "- \\[ *[0-9.s]+\\] " ""
      (replace-regexp-in-string
       "\033\\[0;90m" ""
       (replace-regexp-in-string
        "\\`.*\n" ""
        package-upgrades)))
     nil
     (expand-file-name "misc/upgradable-packages.txt" config-root))
    (publish "misc/upgradable-packages.txt")))

(setq inhibit-message t)
(kill-emacs (doom-cli-execute :doom "check-updates"))
