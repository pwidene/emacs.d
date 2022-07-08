;;
;; OS-specific
;;
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  ;; map RH Option key to Super prefix (s-<key>)
  (setq ns-right-option-modifier 'super)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
)

(defun pmw/proxy-on ()
  "Set SRN proxies"
  (interactive)
  (setq url-proxy-services '(("no_proxy" . "sandia\\.gov")
			     ("http" . "nouser:nopass@proxy.sandia.gov:80")
			     ("https" . "nouser:nopass@proxy.sandia.gov:80")))
  (message "%s" "URL proxies set for SRN."))

(defun pmw/proxy-off ()
  "Un-set SRN proxies"
  (interactive)
  (setq url-proxy-services nil)
  (message "%s" "Proxies un-set."))

;;
;; If we are on the SRN (IPv4 addr begins with 134.253), set URL proxies
;;
;; UPDATE Feb 2021: whatever was broken in (network-interface-list) and/or macOS has apparently
;; been fixed, so I reverted the definition of get-ip-address away from the ifconfig
;; shell-out. The below paragraph is no longer operative.
;;
;; DISREGARD Feb 2021: For some reason (network-interface-list) has stopped working under some combination
;; of Mac OS X High Sierra (and later) and Emacs 25 (and later). The modification of
;; pmw/proxy-check below which now shells out to ifconfig compensates (although fragile
;; because of the cut command).
;;
(defun pmw/get-ip-address ()
  "get the IP address for the first en? interface that has a valid IP address"
  (interactive)
  ;; macOS maintains the network interface of the VPN (10.208.x.x) as long as
  ;; the VPN app is running (even when not connected but still running in menu bar).
  ;; There doesn't appear to be any reliable way to figure out whether the VPN address
  ;; is actually connected from inside Emacs. scutil --nc is no help here because
  ;; Sandia's VPN config runs outside macOS's network config and so configd has no knowledge
  ;; of it. All this is to say that trying to automatically set proxies when on the VPN may be
  ;; possible, but I can't figure out how to do it.
  (catch 'ipaddr
    (dolist (x (network-interface-list))
      (when (and (<= (length (cdr x)) 5) (string-match "en" (car x)))
	(throw 'ipaddr (format-network-address (cdr x) t))))))

;; (let ((dev (if dev dev "en0")))
;;   (format-network-address (car (network-interface-info dev)) t)))

(defun pmw/proxy-check ()
  "Check for SRN IP address and set proxies appropriately if found"
  (interactive)
  ;; 134.253.x.x is SRN, 10.208.x.x is NM VPN
  (when (string-match "134\.253" (pmw/get-ip-address))
    (pmw/proxy-on)))

;;  (catch 'found
;;  (dolist (x (network-interface-list))

;; (dolist (ipaddr (split-string (shell-command-to-string "ifconfig | grep inet | grep -v inet6 | cut -d ' ' -f 2")))
;;      (when (string-match "134\.253" ipaddr)
;;	(throw 'found ipaddr))) ; break the do loop






;;
;; My functions
;;
(defun pmw/new-buffer-frame ()
  "Create a new frame with empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))
(global-set-key (kbd "C-c f") #'pmw/new-buffer-frame)

(defun pmw/find-alternative-file-with-sudo ()
  (interactive)
  (let ((fname (or buffer-file-name
		   dired-directory)))
    (when fname
      (if (string-match "^/sudo:pwidene@localhost:" fname)
	  (setq fname (replace-regexp-in-string
		       "^/sudo:pwidene@localhost:" ""
		       fname))
	(setq fname (concat "/sudo:pwidene@localhost:" fname)))
      (find-alternate-file fname))))
(global-set-key (kbd "C-x C-r") 'pmw/find-alternative-file-with-sudo)

;;; for laptop and anything that might move
(pmw/proxy-check)