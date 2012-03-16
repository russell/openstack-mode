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


(defun openstack-keystone-call (url method &optional kvdata)
  (openstack-call (concat openstack-auth-url url)
                  method
                  `(("x-auth-token" . ,openstack-token)
                    ("Content-Type" . "application/json"))
                  kvdata))

(defun openstack-keystone-auth (kvdata)
  (openstack-call (concat openstack-auth-url "/tokens")
                  "POST"
                  '(("Content-Type" . "application/json"))
                  kvdata))

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
  (openstack-call
   (concat (openstack-service-catalog-filter "compute")
           url)
   method
   `(("x-auth-project-id" . ,(cdr (assoc 'name openstack-tenant)))
     ("x-auth-token" . ,openstack-token)
     ("Content-Type" . "application/json"))
   kvdata))

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
           (data (openstack-keystone-auth
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

(defun openstack-api-details ()
  (let ((data (openstack-nova-call
                "/"
                "GET")))
         data))

(defun openstack-extensions-list ()
  (let ((data (openstack-nova-call
                "/extensions"
                "GET")))
         data))

(defun openstack-extension-details (alias)
  (let ((data (openstack-nova-call
                (format "/extensions/%s" alias)
                "GET")))
         data))

(defun openstack-tenants-list ()
  (let* ((data (cdr (assoc 'tenants
                           (openstack-keystone-call
                            "/tenants"
                            "GET"))))
         (values (cdr (assoc 'values data))))
    values))

(defun* openstack-servers-list (&optional (detail nil))
  (let* ((data (openstack-nova-call
                (concat "/extras/servers")
                "GET")))
         data))

(defun* openstack-server-reboot1 (id &optional &key (type "SOFT"))
  (let* ((data (openstack-nova-call
                (format "/servers/%s/action" id)
                "POST"
                (list :reboot (list :type type)))))))

(defun openstack-server-terminate ()
  (interactive)
  (openstack-nova-call
   (format "/servers/%s" (openstack-instance-id))
   "DELETE")
  (openstack-server-list-all))
