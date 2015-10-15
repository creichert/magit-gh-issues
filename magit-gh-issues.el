;;; magit-gh-issues.el --- GitHub issues extension for Magit

;; Copyright (C) 2015 Christopher Reichert

;; Author: Christopher Reichert <creichert07@gmail.com>
;; Keywords: git tools
;; Version: 0.1
;; URL: https://github.com/creichert/magit-gh-issues
;; Package-Requires: ((emacs "24") (gh "0.4.3") (magit "1.4.0") (pcache "0.2.3") (s "1.6.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This mode is still a work-in-progress, use at your own risk.

;; This is a Magit extension for manipulating GitHub issues

;; No configuration is needed in the repository of any of your remotes contain a
;; URL to Github's remote repository. If for some reason you don't have any
;; Github remotes in your config, you can specify username and repository
;; explicitly:

;; $ git config magit.gh-issues-repo <user>/<repo> # your github repository

;; Add these lines to your init.el:

;; (require 'magit-gh-issues.el)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-issues)

;; There are currently 2 bindings for issues:
;; # g i  --- refreshes the list of issues
;; # g b  --- browse the issue using 'browse-url


;;; Code:

(require 'eieio)
(require 'magit)
(require 'gh-issues)
(require 'gh-users)
;;(require 'gh-issues-comments)

(require 'pcache)
(require 's)

(defvar magit-gh-issues-maybe-filter-issues 'identity
  "Filter function which should validate issues you want to be
  viewed in magit. It receives a list of issues and should
  return a list of issues.")

(defvar magit-gh-issues-collapse-commits t
  "Collapse commits in issues requests listing.")

(defun magit-gh-issues-get-api ()
  (gh-issues-api "api" :sync t :num-retries 1 :cache (gh-cache "cache")))

(defun magit-gh-issues-get-repo-from-config ()
  (let* ((cfg (magit-get "magit" "gh-issues-repo")))
    (when cfg
      (let* ((split (split-string cfg "/")))
        (cons (car split) (cadr split))))))

(defun magit-gh-issues-parse-url (url)
  (let ((creds (s-match "github.com[:/]\\([^/]+\\)/\\([^/]+\\)/?$" url)))
    (when creds
      (cons (cadr creds) (s-chop-suffix ".git" (caddr creds))))))

(defun magit-gh-issues-guess-repo-from-origin ()
  (let ((creds nil))
    (dolist (remote (magit-git-lines "remote") creds)
      (let ((parsed (magit-gh-issues-parse-url
                     (magit-get "remote" remote "url"))))
        (when parsed
          (setq creds parsed))))))

(defun magit-gh-issues-guess-repo ()
  (or (magit-gh-issues-get-repo-from-config)
      (magit-gh-issues-guess-repo-from-origin)))

(defun truncate-string (len s)
  (if (> (length s) len)
      (format "%s.." (substring s 0 (- len 2)))
    s))

(defun magit-gh-issues-insert-gh-issues ()
  (condition-case print-section
      (progn
        (let* ((repo (magit-gh-issues-guess-repo)))
          (when repo
            (let* ((api (magit-gh-issues-get-api))
                   (owner (car repo))
                   (proj (cdr repo))
                   (stubs (funcall magit-gh-issues-maybe-filter-issues
                                   (oref (gh-issues-issue-list api owner proj) :data)))
                   (branch (magit-get-current-branch)))
              (when (> (length stubs) 0)
                (magit-insert-section (stubs)
                  (magit-insert-heading (format "Issues (%s): " (length stubs)))
                  ;; limit the number of items
                  ;; (dolist (stub (subseq stubs 0 (magit-gh-issues-limit)))
                  (dolist (stub stubs)
                    (let* ((id (oref stub :number))
                           (title (oref stub :title))
                           (body (oref stub :body))
                           (user (oref (oref stub :user) :login))
                           (state (oref stub :state))
                           ;; FIX map over all
                           (labels (oref stub :labels))
                           (have-comments t)
                           (header (concat
                                    "["
                                    (propertize (format "#%s" id)
                                                'face '(:foreground "cyan"))
                                    "@"
                                    (format "%-10s " (truncate-string 10 user))
                                    "("
                                    (propertize (format "%s" state)
                                                'face '(:foreground "green")
                                                'font-lock-face '(:weight bold)
                                                ;; face with some options for future reference
                                                'face (cond (nil 'widget-inactive)
                                                            (nil 'error)
                                                            (nil 'italic)
                                                            (t 'default)))

                                    ")] "
                                    (propertize (format " %s" title)
                                                ;;'help-echo (title)
                                                'face (cond (nil 'widget-inactive)
                                                            ;; (have-commits 'default)
                                                            (t 'bold)
                                                            (nil 'error)
                                                            (t 'italic)))
                                    "\n"))
                           (msg (propertize (replace-regexp-in-string "\r$" ""
                                                                      (format "%s\n" body))
                                            ;;'left-margin 30
                                            'face (cond (nil 'widget-inactive)
                                                        ;; (have-commits 'default)
                                                        (t 'default)
                                                        (nil 'error)
                                                        (t 'italic))))

                           (chunks (split-string msg "\n"))
                           ;; info data is stored in the magit section
                           (info (list owner proj id)))
                      ;; format a single section
                      (cond
                       ;; TODO show differently when comments are available
                       ;;(have-comments
                       ;; (magit-with-section (section unfetched-issue info)
                       ;;     (insert header)))
                       (t
                        (magit-insert-section (pull info t)
                          (magit-insert-heading header)
                          (dolist (lbl labels)
                            (insert (propertize (format "\t%s\n" (cdr (assoc 'name lbl)))
                                        'font-lock-face '(:foreground "green"))))
                          (insert "\n")
                          (dolist (chunk chunks)
                            (let* ((beg (point)))
                              (insert chunk)
                              (fill-region-as-paragraph beg (point))
                              (insert "\n"))))))))
                  ;; TODO set a default limit to stop showing issues (e.g. 50),
                  ;; on that number, break from loop and insert some sort of text
                  (when (> (length stubs) 0)
                    (insert "\n"))))))))
    (error "Something went terribly wrong in magit-gh-issues-mode")))

(defun magit-gh-issues-url-for-issue (info)
  "Return github url for an issue request using INFO."
  (let ((url "https://github.com/%s/%s/issues/%s"))
    (apply 'format url info)))

(defun magit-gh-issues-open-in-browser ()
  (interactive)
  (let ((info (magit-section-info (magit-current-section))))
    (browse-url (magit-gh-issues-url-for-issue info))))

(defun magit-gh-issues-purge-cache ()
  (let* ((api (magit-gh-issues-get-api))
         (cache (oref api :cache))
         (repo (magit-gh-issues-guess-repo)))
    (pcache-map cache (lambda (k v)
                        (when (string-match
                               (format "/repos/%s/%s/" (car repo) (cdr repo))
                               (car k))
                          (pcache-invalidate cache k))))))

(defun magit-gh-issues-reload ()
  (interactive)
  (let ((creds (magit-gh-issues-guess-repo)))
    (if (not (and creds (car creds) (cdr creds)))
        (message "Remote repository is not configured or incorrect.")
      (progn
        (turn-on-magit-gh-issues)
        (magit-gh-issues-purge-cache)
        ;;(gh-issues-issue-list (magit-gh-issues-get-api (car creds) (cdr creds)))
        (magit-gh-issues-insert-gh-issues)
        (magit-refresh)
        (message "%s" "Github Issues Sync'ed")))))

(easy-menu-define magit-gh-issues-extension-menu
  nil
  "GitHub Issues extension menu"
  '("GitHub Issues"
    :visible magit-gh-issues-mode
    ["Reload issues request" magit-gh-issues-reload]
    ))

(easy-menu-add-item 'magit-mode-menu
                    '("Extensions")
                    magit-gh-issues-extension-menu)

(defvar magit-gh-issues-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "# g i") 'magit-gh-issues-reload)
    (define-key map (kbd "# g b") 'magit-gh-issues-open-in-browser)
    map))

(defvar magit-gh-issues-mode-lighter " Issues")

;;;###autoload
(define-minor-mode magit-gh-issues-mode "GitHub Issues support for Magit"
  :lighter  magit-gh-issues-mode-lighter
  :require 'magit-gh-issues
  :keymap  'magit-gh-issues-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (if magit-gh-issues-mode
      (magit-add-section-hook
       'magit-status-sections-hook
       'magit-gh-issues-insert-gh-issues
       'magit-insert-stashes)
    (remove-hook 'magit-status-sections-hook 'magit-gh-issues-insert-gh-issues))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(defun turn-on-magit-gh-issues()
  "Unconditionally turn on `magit-issues-mode'."
  (magit-gh-issues-mode 1))

;; Tests
(ert-deftest test-magit-gh-issues-parse-url-git-at ()
  (should (equal '("creichert" . "magit-gh-issues")
                 (magit-gh-issues-parse-url "git@github.com:creichert/magit-gh-issues.git"))))

(ert-deftest test-magit-gh-issues-parse-url-https ()
  (should (equal '("creichert" . "magit-gh-issues")
                 (magit-gh-issues-parse-url "https://github.com/creichert/magit-gh-issues.git"))))

(ert-deftest test-magit-gh-issues-parse-url-https ()
  (should (equal '("creichert" . "magit-gh-issues")
                 (magit-gh-issues-parse-url "https://github.com/creichert/magit-gh-issues/"))))

(ert-deftest test-magit-gh-issues-parse-url-http ()
  (should (equal '("creichert" . "magit-gh-issues")
                 (magit-gh-issues-parse-url "http://github.com/creichert/magit-gh-issues.git"))))

(ert-deftest test-magit-gh-issues-parse-url-git ()
  (should (equal '("creichert" . "magit-gh-issues")
                 (magit-gh-issues-parse-url "git://github.com/creichert/magit-gh-issues.git"))))

(ert-deftest test-magic-gh-pulls-parse-url-invalid ()
  (should (eq nil (magit-gh-issues-parse-url "http://google.com"))))

(ert-deftest test-magic-gh-pulls-parse-url-garbage ()
  (should (eq nil (magit-gh-issues-parse-url "sfalwkerwe09"))))


(provide 'magit-gh-issues)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-gh-issues.el ends here
