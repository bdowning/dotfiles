(require 'org)

(require 'org-mobile)
(defun org-mobile-get-outline-path-link (pom)
  (org-with-point-at pom
    (concat "olp:"
	    (org-mobile-escape-olp (file-name-nondirectory buffer-file-name))
	    ":"
	    (mapconcat 'org-mobile-escape-olp
		       (org-get-outline-path)
		       "/")
	    "/"
	    (org-mobile-escape-olp (nth 4 (org-heading-components))))))

(setq org-directory "~/Dropbox/org")
(setq org-agenda-files
      '("~/Dropbox/org/gtd.org"))
(setq org-stuck-projects
      '("TODO=\"PROJ\""
        ("NEXT")
        nil ""))

(setq org-agenda-custom-commands
      '(("o" "Errands out (@town)" tags-todo "@town")
        ("p" "Phone (@phone)" tags-todo "@phone")
        ("r" "MRM (@mrm)" tags-todo "@mrm")))
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org")
(setq org-mobile-force-id-on-agenda-items nil)
(setq org-mobile-agendas '("a" "p" "o" "r"))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
