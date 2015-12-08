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

(defun org-dblock-write:rangereport (params)
  "Display day-by-day time reports."
  (let* ((ts (plist-get params :tstart))
         (te (plist-get params :tend))
         (start (time-to-seconds
                 (apply 'encode-time (org-parse-time-string ts))))
         (end (time-to-seconds
               (apply 'encode-time (org-parse-time-string te))))
         day-numbers)
    (setq params (plist-put params :tstart nil))
    (setq params (plist-put params :end nil))
    (while (<= start end)
      (save-excursion
        (insert "\n\n"
                (format-time-string (car org-time-stamp-formats)
                                    (seconds-to-time start))
                "----------------\n")
        (org-dblock-write:clocktable
         (plist-put
          (plist-put
           params
           :tstart
           (format-time-string (car org-time-stamp-formats)
                               (seconds-to-time start)))
          :tend
          (format-time-string (car org-time-stamp-formats)
                              (seconds-to-time end))))
        (setq start (+ 86400 start))))))
