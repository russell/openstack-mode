;;; osapi-mode.el --- an rpc layer for openstack os-api

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author   : Russell Sim <russell.sim@gmail.com>
;; URL      : https://github.com/russell/openstack-mode
;; Version  : 0.1
;; Keywords : openstack, rpc

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

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
  (when (yes-or-no-p
         (format "are you sure you would like to terminate instance %s "
                 (openstack-instance-id)))
    (osapi-nova-call
     (format "/servers/%s" (openstack-instance-id))
     "DELETE")
    (openstack-server-list-all)))


(provide 'osapi)
