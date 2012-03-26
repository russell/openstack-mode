
(eval-when-compile (require 'cl))
(require 'openstack-api)

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

(defcustom openstack-instance-display '((:title ec2 :eval (format "i-%08X" (openstack-multi-assoc 'id item)))
                                        (:title name :eval (propertize (format "%s" (cdr (assoc 'name item))) 'face 'openstack-title-face))
                                        user_id (attrs host) status
                                        )
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

(defconst openstack-console-log-buffer "*Openstack: Console Log*"
  "Openstack console log buffer name.")

(defvar openstack-instance nil
  "the openstack instance.")

(defvar openstack-form-widgets nil
  "A table for lookup widget created in current buffer.")

(defun openstack-align ()
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (align-regexp (point) (point-max)
                    "\\(\\s-*\\)|" 1 1 t))))

(defun openstack-multi-assoc (keys alist)
  (if (not (listp keys))
      (assoc-default keys alist)
    (let ((current-alist alist)
          (remaining-keys keys))
      (while (> (length remaining-keys) 1)
        (setq current-alist (assoc-default (car remaining-keys) current-alist))
        (setq remaining-keys (cdr remaining-keys)))
      (assoc-default (car remaining-keys) current-alist))))

(defun openstack-buffer-setup ()
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays))

(defun openstack-form-create (id widget)
  (if (assoc id openstack-form-widgets)
      (error "identifier %S is used!" id)
    (push (cons id widget) openstack-form-widgets)))

(defun openstack-server-reboot ()
  (interactive)
  (openstack-server-reboot1 (openstack-instance-id))
  (openstack-server-list-all))

(defun openstack-server-list-all ()
  (interactive)
  (when (get-buffer openstack-buffer)
    (set-buffer openstack-buffer)
    (let* ((data (openstack-servers-list))
           (current-point (point))
           (inhibit-read-only t))
      (goto-line 3)
      (delete-region (point) (point-max))
      (openstack-headings-widgets)
      (loop for instance across (cdr (assoc 'servers data))
            do (openstack-item-widget instance))
      (openstack-align)
      (goto-char current-point))))

(defun* openstack-server-console-log ()
  (interactive)
  (let ((instance-id (openstack-instance-id)))
        (switch-to-buffer openstack-console-log-buffer)
        (when (get-buffer openstack-console-log-buffer)
          (set-buffer openstack-console-log-buffer)
          (let* ((data (openstack-nova-call
                        "/extras/consoles"
                        "POST"
                        (list :console (list :type "text"
                                             :server_id instance-id))))
                 (current-point (point))
                 (inhibit-read-only t))
            (insert (assoc-default 'output (assoc 'console data)))
            (goto-char (point-min))
            (replace-string "" ""))
          (use-local-map openstack-mode-map)
          (setq mode-name "Console-Log")
          (setq buffer-read-only t)
          (reposition-window))))

(defun openstack-buffer-heading ()
  (openstack-form-create
   'tenant
   (widget-create 'editable-field
                  :action (lambda (wid &rest ignore)
                            (message (widget-value wid))
                            (openstack-token-init (widget-value wid))
                            (openstack-server-list-all))
                  :format "Tenant: %v"
                  :help-echo "TAB: complete field; RET: enter value"
                  :complete-function #'openstack-complete-tenant
                  :keymap (let ((map (copy-keymap widget-field-keymap)))
                            (define-key map "\t" 'widget-complete)
                            map)
                  openstack-default-tenant))
  ;;   (propertize "pt-89" 'face 'openstack-header-face))
  (widget-insert "\n")
  (widget-setup))

(defun* openstack-complete-tenant ()
  (interactive)
  (let* ((predicate (widget-value (widget-at)))
         (beg (save-excursion
               (search-backward predicate)))
         (tenants-alist (let ((count 0))
                          (loop for tenant across (openstack-tenants-list)
                                do (setq count (1+ count))
                                collect (list (cdr (assoc 'name tenant)) count)))))
    (completion-in-region beg (point) tenants-alist)))

(defun openstack-instance-display-header (item)
  (cond
   ((and (listp item) (eq :title (car item)))
    (second item))
   (t
    (last-element item))))

(defun openstack-instance-display-widgets (element item)
  (cond
   ((and (listp element) (eq :title (car element)))
    (destructuring-bind (&key title eval)
        element
      (eval eval)))
   (t
    (openstack-multi-assoc element item))))

(defun openstack-headings-widgets ()
  (flet ((last-element (element)
                       (if (listp element)
                           (car (last element))
                         element)))

  (widget-insert (format " %s" 'id))
  (dolist (element openstack-instance-display)
    (widget-insert " | ")
    (widget-insert (format "%s"
                           (openstack-instance-display-header element))))
  (widget-insert "\n")
  (widget-insert (format " %s" (make-string (length (symbol-name 'id)) ?-)))
  (dolist (element openstack-instance-display)
    (widget-insert " | ")
    (widget-insert
     (format
      "%s"
      (make-string
       (length
        (symbol-name
         (openstack-instance-display-header element)))
       ?-))))
  (widget-insert "\n")))


(defun* openstack-item-widget (item &optional (mark " "))
  (widget-insert mark)
  (widget-insert (format "%s"
                         (cdr (assoc 'id item))))
  (dolist (element openstack-instance-display)
    (widget-insert " | ")
    (widget-insert
     (let ((value (openstack-instance-display-widgets element item)))
       (if (stringp value)
           value
           (format "%s" value)))))
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
  (if
      (get-text-property (line-beginning-position)
                         'openstack-properties)
      (let ((inhibit-read-only t))
        (openstack-set-mark-1 mark)
        (openstack-redisplay-current)
        (beginning-of-line))))

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
  "Like `ido-find-file', but default to the directory of the
instance at point."
  (interactive)
  (let ((ip (car (openstack-instance-ip-addresses
                  (get-text-property (line-beginning-position)
                                     'openstack-properties)))))
    (if ip
        (ido-find-file-in-dir (concat "/" ip ":"))
      (ido-find-file))))

(defun openstack-kill-buffer ()
  (interactive)
  (kill-buffer))

(defvar openstack-mode-map
  nil
  "Keymap for Openstack mode.")

(setq openstack-mode-map
      (let ((map (make-keymap)))
        (define-key map (kbd "RET") 'openstack-instance-details)
        (define-key map "g" 'openstack-server-list-all)
        (define-key map "n" 'openstack-forward-line)
        (define-key map "m" 'openstack-mark-forward)
        (define-key map "p" 'openstack-backward-line)
        (define-key map "u" 'openstack-unmark-forward)
        (define-key map "q" 'openstack-kill-buffer)
        (define-key map "l" 'openstack-server-console-log)
        (define-key map "R" 'openstack-server-reboot)
        (define-key map "K" 'openstack-server-terminate)
        (define-key map (kbd "C-x C-f") 'openstack-ido-find-file)
        map))

(defvar openstack-console-log-mode-map
  nil
  "Keymap for Openstack mode.")

(setq openstack-console-log-mode-map
      (let ((map (make-keymap)))
        (define-key map "q" 'openstack-kill-buffer)
        map))

(defun openstack-mode ()
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map openstack-mode-map)
  (setq major-mode 'openstack-mode)
  (make-local-variable 'openstack-form-widgets)
  (setq mode-name "Openstack")
  (openstack-buffer-setup)
  (openstack-token-init)
  (openstack-buffer-heading)
  (openstack-server-list-all))

(put 'openstack-mode 'mode-class 'special)

(defun openstack ()
  (interactive)
  (switch-to-buffer openstack-buffer)
  (if (not (eq major-mode 'openstack-mode))
      (openstack-mode)))

(provide 'openstack-mode)
