(eval-when-compile (require 'cl))


(defvar osapi-token nil
  "the openstack token.")

(defvar osapi-token-expiry nil
  "the openstack token.")

(defvar osapi-service-catalog nil
  "the openstack service catalog.")

(defun osapi-keystone-call (url method &optional kvdata)
  (osapi-call (concat openstack-auth-url url)
                  method
                  `(("x-auth-token" . ,osapi-token)
                    ("Content-Type" . "application/json"))
                  kvdata))

(defun osapi-keystone-auth (kvdata)
  (osapi-call (concat openstack-auth-url "/tokens")
                  "POST"
                  '(("Content-Type" . "application/json"))
                  kvdata :disable-token-refresh t))

(defun osapi-parse ()
  "Parse the result of a openstack request."
  (goto-char (point-min))
  (condition-case nil
      (when (search-forward-regexp "^$" nil t)
        (json-read))
    (error
     (message "openstack: Could not read the response.")
     nil)))

(defun osapi-call (url method headers
                           &optional kvdata
                           &key disable-token-refresh)
  (when (and
         (not disable-token-refresh)
         (<
          (time-to-seconds (time-subtract
                            (date-to-time osapi-token-expiry)
                            (current-time)))
          100) ; if there is less that 10min til expiry then renew.
         (message "Refreshing Openstack Token")
         (osapi-token-init)))
  (let ((url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data (if kvdata (json-encode kvdata) nil))
        (url-extensions-header nil))
    (with-current-buffer (url-retrieve-synchronously url)
      (point-min)
      (if (or (equal url-http-response-status 204)
              (equal url-http-content-length 0))
          (kill-buffer (current-buffer))
        (let ((data (osapi-parse)))
          (kill-buffer (current-buffer))
          data)))))


(defun osapi-nova-call (url method &optional kvdata)
  (osapi-call
   (concat (osapi-service-catalog-filter "compute")
           url)
   method
   `(("x-auth-project-id" . ,(cdr (assoc 'name openstack-tenant)))
     ("x-auth-token" . ,osapi-token)
     ("Content-Type" . "application/json"))
   kvdata))

(defun* osapi-service-catalog-filter (type &optional &key
                                               region
                                               (url 'publicURL))
  "return the first url that matches the filter"
  (car
   (loop for endpoint across
         (cdr (assoc 'endpoints
                     (car
                      (loop for x across osapi-service-catalog
                            when (equal (cdr (assoc 'type x)) type)
                            collect x))))
         when (if region (equal (cdr (assoc 'region endpoint)) region)
                t)
         collect (cdr (assoc url endpoint)))))

(defun osapi-token-init (&optional tenant)
  (when (get-buffer openstack-buffer)
    (set-buffer openstack-buffer)
    (let* ((tenant (if tenant
                       tenant
                     openstack-default-tenant))
           (data (osapi-keystone-auth
                  (list
                   :auth
                   (list
                    :tenantName tenant
                    :passwordCredentials
                    (list
                     :username openstack-username
                     :password openstack-password))))))
      (let ((access (assoc 'access data)))
        (setq osapi-service-catalog (cdr (assoc 'serviceCatalog access))
              openstack-user (cdr (assoc 'user access))
              osapi-token (cdr (assoc 'id (assoc 'token access)))
              osapi-token-expiry (cdr (assoc 'expires (assoc 'token access)))
              openstack-tenant (cdr (assoc 'tenant (assoc 'token access))))))))

(defun osapi-api-details ()
  (let ((data (osapi-nova-call
                "/"
                "GET")))
         data))

(defun osapi-extensions-list ()
  (let ((data (osapi-nova-call
                "/extensions"
                "GET")))
         data))

(defun osapi-extension-details (alias)
  (let ((data (osapi-nova-call
                (format "/extensions/%s" alias)
                "GET")))
         data))

(defun osapi-tenants-list ()
  (let* ((data (cdr (assoc 'tenants
                           (osapi-keystone-call
                            "/tenants"
                            "GET"))))
         (values (cdr (assoc 'values data))))
    values))

(defun* osapi-servers-list (&optional (detail nil))
  (let* ((data (osapi-nova-call
                (concat "/admin/servers")
                "GET")))
         data))

(defun* osapi-server-reboot (id &optional &key (type "SOFT"))
  (let* ((data (osapi-nova-call
                (format "/servers/%s/action" id)
                "POST"
                (list :reboot (list :type type)))))))

(defun osapi-server-terminate ()
  (interactive)
  (osapi-nova-call
   (format "/servers/%s" (openstack-instance-id))
   "DELETE")
  (openstack-server-list-all))


(provide 'osapi)
