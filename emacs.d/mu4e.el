(require 'mu4e)
(require 'org-mu4e)
(require 'mu4e-contrib)

(require 'shr)
(setq mu4e-html2text-command 'mu4e-shr2text)
(defun shr-colorize-region (start end fg &optional bg)
  nil)

(setq mu4e-view-show-addresses t)

(setq mu4e-bookmarks
      '(("(maildir:/bdowning@lavos.net/INBOX OR tag:\\\\Inbox)"
         "All inboxes" ?i)
        ("(flag:unread AND NOT flag:trashed)" "Unread messages"      ?u)
        ("date:today..now"                  "Today's messages"     ?t)
        ("date:7d..now"                     "Last 7 days"          ?w)
        ("mime:image/*"                     "Messages with images" ?p)))

(setq mu4e-user-mail-address-list
      '("bdowning@lavos.net"
        "phs2501@gmail.com"
        "bdowning@mrym.org"
        "brian.downing@mrym.org"
        "brian.downing@gld-i.com"))

(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"
      message-sendmail-extra-arguments '("--read-envelope-from"))

(setq bd/mu4e-account-alist
  '(("bdowning@lavos.net"
     (user-mail-address . "bdowning@lavos.net")
     (mu4e-drafts-folder . "/bdowning@lavos.net/Drafts")
     (mu4e-sent-folder . "/bdowning@lavos.net/Sent")
     (mu4e-sent-messages-behavior . sent)
     (mu4e-trash-folder . "/bdowning@lavos.net/Trash")
     (message-sendmail-extra-arguments . ("-a" "lavos")))
    ("phs2501@gmail.com"
     (user-mail-address . "phs2501@gmail.com")
     (mu4e-drafts-folder . "/phs2501@gmail.com/Drafts")
     (mu4e-sent-folder . "")
     (mu4e-sent-messages-behavior . delete)
     (mu4e-trash-folder . "/phs2501@gmail.com/Trash")
     (message-sendmail-extra-arguments . ("-a" "gmail")))
    ("bdowning@mrym.org"
     (user-mail-address . "bdowning@mrym.org")
     (mu4e-drafts-folder . "/bdowning@mrym.org/Drafts")
     (mu4e-sent-folder . "")
     (mu4e-sent-messages-behavior . delete)
     (mu4e-trash-folder . "/bdowning@mrym.org/Trash")
     (message-sendmail-extra-arguments . ("-a" "mrym")))))

(defun bd/mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
              (completing-read (format "Compose with account: (%s) "
                                       (mapconcat #'(lambda (var) (car var))
                                                  bd/mu4e-account-alist "/"))
                               (mapcar #'(lambda (var) (car var)) bd/mu4e-account-alist)
                               nil t nil nil (caar bd/mu4e-account-alist))))
         (account-vars (cdr (assoc account bd/mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cdr var)))
              account-vars)
        (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'bd/mu4e-set-account)

(setq bd/mu4e-refiles
      '(("/bdowning@lavos.net/"
         ("Archive.sysadmin" :subject ("Debian package update(s)" "apt-listchanges"))
         ("Bulk.AOPA" :from "aopa.org$")
         ("Bulk.Amazon" :from "amazon.com$")
         ("Bulk.Ameritrade" :from "ameritrade.com$")
         ("Bulk.Arrow" :from "arrow.com$")
         ("Bulk.Bank of America" :from "bankofamerica.com$")
         ("Bulk.Bechtel" :from "bechtelhomes.com$")
         ("Bulk.Best Buy" :from "bestbuy.com$")
         ("Bulk.Borders" :from ("bn.com$" "borders.com$"))
         ("Bulk.Busey" :from "busey.com$")
         ("Bulk.CCHS" :from "cuhumane.org$")
         ("Bulk.Chase" :from "chase.com$")
         ("Bulk.EVE" :from "eveonline.com$")
         ("Bulk.Facebook" :from "facebook\\(mail\\)?.com$")
         ("Bulk.Funcom" :from "funcom.com$")
         ("Bulk.GOG" :from "gog.com$")
         ("Bulk.GoDaddy" :from "godaddy.com$")
         ("Bulk.HSBC" :from "hsbc.com$")
         ("Bulk.Hitbox" :from "hitbox.tv$")
         ("Bulk.Hurricane Electric" :from "he.net$")
         ("Bulk.Name Dot Com" :from "name.com$")
         ("Bulk.Netflix" :from "netflix.com$")
         ("Bulk.Newegg" :from "newegg.com$")
         ("Bulk.PCF" :from "preventcancer.org$")
         ("Bulk.Papa Johns" :from "papajohns\\(-specials\\).com$")
         ("Bulk.PayPal" :from "paypal.com$")
         ("Bulk.Penny Arcade" :from "penny-arcade.com$")
         ("Bulk.SourceForge" :from "sourceforge.net$")
         ("Bulk.Station Theatre" :from "stationtheatrepublicity@gmail.com$")
         ("Bulk.Steam" :from "steampowered.com$")
         ("Bulk.Straight Talk" :from "straighttalk.com$")
         ("Bulk.SystemInPlace" :from "systeminplace.net$")
         ("Bulk.T-Mobile" :from "t-mobile-email.com$")
         ("Bulk.Turbine" :from "turbine.com$")
         ("Bulk.TurboTax" :from ("turbotax.com$" "intuit.com$"))
         ("Bulk.Twitch" :from "twitch.tv$")
         ("Bulk.VCA" :from "vetstreetmail.com$")
         ("Bulk.Walgreens" :from "walgreens.com$")
         ("Bulk.Wells Fargo" :from "wellsfargo.com$")
         ("Bulk.eBay" :from "ebay.com$")
         ("Bulk.gogoNET" :from "gogo6.com$")
         ("Lists.MRM-L" :mailing-list "MRM-L.yahoogroups.com")
         ("Lists.arch-announce" :mailing-list "arch-announce.archlinux.org")
         )))
(defun bd/mu4e-message-field-match (msg field regexps)
  (block nil
    (when (not (listp regexps))
      (setq regexps (list regexps)))
    (dolist (regexp regexps)
      (let ((field-value (mu4e-message-field msg field)))
        (when (and field-value (string-match regexp field-value))
          (return t))))))
(defun bd/mu4e-refile-fn (msg)
  (block out
    (dolist (maildir bd/mu4e-refiles)
      (when (string-prefix-p (first maildir) (mu4e-message-field msg :maildir))
        (dolist (test (rest maildir))
          (destructuring-bind (folder field regexps) test
            (case field
              ((:to :from :cc :bcc)
               (when (mu4e-message-contact-field-matches msg field regexps)
                 (return-from out (concat (first maildir) folder))))
              (otherwise
               (when (bd/mu4e-message-field-match msg field regexps)
                 (return-from out (concat (first maildir) folder)))))))))
    (cond ((bd/mu4e-message-field-match msg :maildir "^/bdowning@lavos.net")
           "/bdowning@lavos.net/Archive")
          ((bd/mu4e-message-field-match msg :maildir "^/phs2501@gmail.com")
           "/phs2501@gmail.com/All Mail")
          ((bd/mu4e-message-field-match msg :maildir "^/bdowning@mrym.org")
           "/bdowning@mrym.org/All Mail"))))

(setq mu4e-refile-folder #'bd/mu4e-refile-fn)

(setq mu4e-update-interval 300
      mu4e-get-mail-command "offlineimap")
