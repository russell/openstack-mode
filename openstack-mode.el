
(eval-when-compile (require 'cl))

(defgroup openstack-mode nil
  "Openstack mode"
  :group 'local)

(defcustom openstack-auth-url "http://localhost:5000/v2.0"
  "Openstack keystone url."
  :type 'string
  :safe 'stringp
  :group 'openstack-mode)

(defcustom openstack-username ""
  "Openstack user."
  :type 'string
  :safe 'stringp
  :group 'openstack-mode)

(defcustom openstack-password ""
  "Openstack password."
  :type 'string
  :safe 'stringp
  :group 'openstack-mode)

(defcustom openstack-default-tenant ""
  "Openstack default tenant."
  :type 'string
  :safe 'stringp
  :group 'openstack-mode)

(defcustom openstack-instance-display '(user_id status)
  "a list of extra columns to display."
  :type 'sexp
  :group 'openstack-mode)

(defcustom openstack-marked-char ?>
  "The character to display for marked buffers."
  :type 'character
  :group 'openstack-mode)

(defgroup openstack-faces nil
  "Faces used in Openstack mode"
  :group 'openstack-mode
  :group 'faces)

(defface openstack-header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for Openstack header."
  :group 'openstack-faces)

(defface openstack-title-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for Openstack file titles."
  :group 'openstack-faces)

(defconst openstack-buffer "*Openstack*"
  "Openstack buffer name.")

(defvar openstack-token nil
  "the openstack token.")

(defvar openstack-token-expiry nil
  "the openstack token.")

(defvar openstack-service-catalog nil
  "the openstack service catalog.")

(defvar openstack-user nil
  "the openstack user.")

(defvar openstack-tenant nil
  "the openstack tenant.")

(defun openstack-parse ()
  "Parse the result of a openstack request."
  (goto-char (point-min))
  (condition-case nil
      (when (search-forward-regexp "^$" nil t)
        (json-read))
    (error
     (message "openstack: Could not read the response.")
     nil)))

(defun openstack-call (url method headers &optional kvdata)
    (let ((url-request-method method)
          (url-request-extra-headers headers)
          (url-request-data (if kvdata (json-encode kvdata) nil))
          (url-extensions-header nil))
      (with-current-buffer (url-retrieve-synchronously url)
        (point-min)
        (if (or (equal url-http-response-status 204)
                (equal url-http-content-length 0))
            (kill-buffer (current-buffer))
          (let ((data (openstack-parse)))
            (kill-buffer (current-buffer))
            data)))))

(defun openstack-nova-call (url method &optional kvdata)
  (openstack-call url method
                  `(("x-auth-project-id" . ,(cdr (assoc 'name openstack-tenant)))
                    ("x-auth-token" . ,openstack-token)
                    ("Content-Type" . "application/json"))
                  kvdata))

(defun openstack-align ()
  (let ((inhibit-read-only t))
    (align (point-min) (point-max)
           'entire
           (list '(text-column
                   ;; TODO update re to include custom mark character
                   (regexp . "\\(^>\\?\\|\\S-\\)\\([ 	]+\\)\\(\\S-\\|$\\)")
                   (group . 2)
                   (modes . '(openstack-mode))
                   (repeat . t)
                   )))))

(defun openstack-buffer-setup ()
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays))

(defun* openstack-service-catalog-filter (type &optional &key
                                               region
                                               (url 'publicURL))
  "return the first url that matches the filter"
  (car
   (loop for endpoint across
         (cdr (assoc 'endpoints
                     (car
                      (loop for x across openstack-service-catalog
                            when (equal (cdr (assoc 'type x)) type)
                            collect x))))
         when (if region (equal (cdr (assoc 'region endpoint)) region)
                t)
         collect (cdr (assoc url endpoint)))))

(defun openstack-token-init (&optional tenant)
  (when (get-buffer openstack-buffer)
    (set-buffer openstack-buffer)
    (let* ((tenant (if tenant
                       tenant
                       openstack-default-tenant))
           (data (openstack-call
                  (concat openstack-auth-url "/tokens")
                  "POST"
                  '(("Content-Type" . "application/json"))
                  (list
                   :auth
                   (list
                    :tenantName tenant
                    :passwordCredentials
                    (list
                     :username openstack-username
                     :password openstack-password))))))
      (let ((access (assoc 'access data)))
        (setq openstack-service-catalog (cdr (assoc 'serviceCatalog access))
              openstack-user (cdr (assoc 'user access))
              openstack-token (cdr (assoc 'id (assoc 'token access)))
              openstack-token-expiry (cdr (assoc 'expires (assoc 'token access)))
              openstack-tenant (cdr (assoc 'tenant (assoc 'token access))))))))

(defun* openstack-server-list (&optional &key detail)
  (when (get-buffer openstack-buffer)
    (set-buffer openstack-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (let* ((data (openstack-nova-call
                  (concat (openstack-service-catalog-filter "compute")
                          "/servers"
                          (when detail "/detail"))
                  "GET")))
      (openstack-buffer-heading)
      (openstack-headings-widgets)
      (loop for instance across (cdr (assoc 'servers data))
            do (openstack-item-widget instance)))
    (openstack-align)))

(defun* openstack-server-reboot1 (id &optional &key (type "SOFT"))
    (let* ((data (openstack-nova-call
                  (concat (openstack-service-catalog-filter "compute")
                          "/servers/" (format "%s" id) "/action")
                  "POST"
                 (list :reboot (list :type type)))))))

(defun openstack-server-reboot ()
  (interactive)
  (openstack-server-reboot1 (openstack-instance-id))
  (openstack-server-list-all))

(defun openstack-server-terminate ()
  (interactive)
  (openstack-nova-call
   (concat (openstack-service-catalog-filter "compute")
           "/servers/" (format "%s" (openstack-instance-id)))
   "DELETE")
  (openstack-server-list-all))

(defun openstack-server-list-all ()
  (interactive)
  (openstack-server-list :detail t))

(defun openstack-buffer-heading ()
  (widget-insert
   ;TODO replace with tenant variable
   (propertize "pt-89" 'face 'openstack-header-face))
  (widget-insert "\n\n"))

(defun openstack-headings-widgets ()
  (dolist (element (cons 'id (cons 'name openstack-instance-display)))
    (widget-insert " ")
    (widget-insert (format "%s"
                           element)))
  (widget-insert "\n")
  (dolist (element (cons 'id (cons 'name openstack-instance-display)))
    (widget-insert " ")
    (widget-insert (format "%s"
                           (make-string (length (symbol-name element)) ?-))))
  (widget-insert "\n"))

(defun* openstack-item-widget (item &optional (mark " "))
  (widget-insert mark)
  (widget-insert (format "%s"
                         (cdr (assoc 'id item))))
  (widget-insert " ")
  (widget-insert (propertize
                  (format "%s"
                          (cdr (assoc 'name item)))
                  'face 'openstack-title-face))
  (dolist (element openstack-instance-display)
    (widget-insert " ")
    (widget-insert (format "%s"
                           (cdr (assoc element item)))))
  (widget-insert "\n")
  (save-excursion
    (let ((inhibit-read-only t))
      (forward-line -1)
      (put-text-property (line-beginning-position)
                         (line-end-position)
                         'openstack-properties item)
      (put-text-property (line-beginning-position)
                         (line-end-position)
                         'openstack-mark mark))))

(defun openstack-instance-id ()
  (cdr (assoc 'id (get-text-property (line-beginning-position)
                                     'openstack-properties))))

(defun openstack-instance-ip-addresses (instance)
  (loop for ip across
        (car
         (loop for address in (cdr (assoc 'addresses instance))
               collect (cdr address)))
        collect (cdr (assoc 'addr ip))))

(defun openstack-forward-line ()
  (interactive)
  (forward-line 1))

(defun openstack-backward-line ()
  (interactive)
  (forward-line -1))

(defun openstack-mark-forward (arg)
  "Mark the buffer on this line, and move forward ARG lines.
If point is on a group name, this function operates on that group."
  (interactive "P")
  (openstack-set-mark openstack-marked-char))

(defun openstack-unmark-forward (arg)
  "Unmark the buffer on this line, and move forward ARG lines.
If point is on a group name, this function operates on that group."
  (interactive "P")
  (openstack-set-mark ?\s))

(defun openstack-set-mark (mark)
  (let ((inhibit-read-only t))
    (openstack-set-mark-1 mark)
    (openstack-redisplay-current)
    (beginning-of-line)))

(defun openstack-set-mark-1 (mark)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (put-text-property beg end 'openstack-mark
                       mark)))

(defun openstack-redisplay-current ()
  (beginning-of-line)
  (let ((mark (get-text-property (line-beginning-position)
                                 'openstack-mark))
        (properties (get-text-property (line-beginning-position)
                                       'openstack-properties)))
    (save-excursion
      (delete-region (point) (1+ (line-end-position)))
      (openstack-item-widget properties mark)
      (forward-line -1))
  (openstack-align)))

(defun openstack-ido-find-file ()
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive)
  (let ((ip (car (openstack-instance-ip-addresses
                  (get-text-property (line-beginning-position)
                                          'openstack-properties)))))
    (if ip
        (ido-find-file-in-dir (concat "/" ip ":"))
      (message "Instance has no ip address"))))

(defvar openstack-mode-map
  (let ((map (make-keymap)))
    (define-key map "g" 'openstack-server-list-all)
    (define-key map "n" 'openstack-forward-line)
    (define-key map "m" 'openstack-mark-forward)
    (define-key map "p" 'openstack-backward-line)
    (define-key map "u" 'openstack-unmark-forward)
    (define-key map "R" 'openstack-server-reboot)
    (define-key map "K" 'openstack-server-terminate)
    (define-key map (kbd "C-x C-f") 'openstack-ido-find-file)
    map)
  "Keymap for Openstack mode.")

(setq openstack-mode-map
 (let ((map (make-keymap)))
    (define-key map "g" 'openstack-server-list-all)
    (define-key map "n" 'openstack-forward-line)
    (define-key map "m" 'openstack-mark-forward)
    (define-key map "p" 'openstack-backward-line)
    (define-key map "u" 'openstack-unmark-forward)
    (define-key map "R" 'openstack-server-reboot)
    (define-key map "K" 'openstack-server-terminate)
    (define-key map (kbd "C-x C-f") 'openstack-ido-find-file)
    map))

(defun openstack-mode ()
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (use-local-map openstack-mode-map)
  (setq major-mode 'openstack-mode)
  (setq mode-name "Openstack")
  (openstack-buffer-setup)
  (openstack-token-init)
  (openstack-server-list-all))

(put 'openstack-mode 'mode-class 'special)

(defun openstack ()
  (interactive)
  (switch-to-buffer openstack-buffer)
  (if (not (eq major-mode 'openstack-mode))
      (openstack-mode)))

(provide 'openstack-mode)
