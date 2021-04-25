;;; graphql-doc.el --- GraphQL Doc -*- lexical-binding: t -*-

;; Copyright (c) 2021 Ian Fitzpatrick <itfitzpatrick@gmail.com>

;; URL: https://github.com/ifitzpatrick/graphql-doc
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Author: Ian Fitzpatrick
;; Created: April 25, 2021
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (request "0.3.2") (promise "1.1"))

;;; Commentary:
;; GraphQL Documentation explorer

;;; Code:

(require 'cl-lib)
(require 'promise)
(require 'request)

(defvar graphql-doc--introspection-query
"query IntrospectionQuery {
  __schema {
    queryType { name }
    mutationType { name }
    subscriptionType { name }
    types {
      ...FullType
    }
    directives {
      name
      description
      
      locations
      args {
        ...InputValue
      }
    }
  }
}

fragment FullType on __Type {
  kind
  name
  description
  
  fields(includeDeprecated: true) {
    name
    description
    args {
      ...InputValue
    }
    type {
      ...TypeRef
    }
    isDeprecated
    deprecationReason
  }
  inputFields {
    ...InputValue
  }
  interfaces {
    ...TypeRef
  }
  enumValues(includeDeprecated: true) {
    name
    description
    isDeprecated
    deprecationReason
  }
  possibleTypes {
    ...TypeRef
  }
}

fragment InputValue on __InputValue {
  name
  description
  type { ...TypeRef }
  defaultValue
}

fragment TypeRef on __Type {
  kind
  name
  ofType {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
              }
            }
          }
        }
      }
    }
  }
}")

(defun graphql-doc--request (url data)
  "Helper function to make http requests to graphql endpoints."
  (promise-new
   (lambda (resolve reject)
     (request
       url
       :type "POST"
       :parser 'json-read
       :data
       data
       :error
       (cl-function
        (lambda (&key response &allow-other-keys)
          (funcall reject (request-response-data response))))
       :success
       (cl-function
        (lambda (&key response &allow-other-keys)
          (funcall resolve (request-response-data response))))))))

(defvar-local graphql-doc--introspection-results nil)

(defun graphql-doc--request-introspection (api)
  "Request introspection query from API."
  (promise-chain
      (graphql-doc--request
       (plist-get api :url)
       (append
        (plist-get api :data)
        `(("variables" . "")
          ("query" . ,graphql-doc--introspection-query))))
    (then (lambda (data)
            (setq-local graphql-doc--introspection-results data)))
    (promise-catch (lambda (data)
                     (message "error: %s" data)))))

(defcustom graphql-doc-apis nil
  "alist mapping name to an api plist")

(defun graphql-doc-add-api (name api)
  "Add an entry (NAME . API) to apis alist."
  (add-to-list 'graphql-doc-apis `(,name . ,api)))

(defun graphql-doc--get-api (name)
  "Get API plist out of graphql-doc-apis."
  (cdr (assoc name graphql-doc-apis)))

(defun graphql-doc--get (key-list list)
  "Follow KEY-LIST to get property out of LIST."
  (if (and key-list list)
      (graphql-doc--get (cdr key-list) (assq (car key-list) list))
    (cdr list)))

(defun graphql-doc--get-types ()
  "Get info about types supported by endpoint."
  (graphql-doc--get '(data __schema types) graphql-doc--introspection-results))

(defun graphql-doc--get-type (name)
  "Get info about type NAME."
  (seq-find
   (lambda (type) (equal name (graphql-doc--get '(name) type)))
   (graphql-doc--get-types)))

(defun graphql-doc--queries ()
  "Get info about queries supported by endpoint."
  (seq-find
   (lambda (type) (equal (graphql-doc--get '(name) type) "Query"))
   (graphql-doc--get-types)))

(defun graphql-doc--mutations ()
  "Get info about mutations supported by endpoint."
  (seq-find
   (lambda (type) (equal (graphql-doc--get '(name) type) "Mutation"))
   (graphql-doc--get-types)))

(defvar-local graphql-doc--history nil
  "List of cons cells with a name and callback that can redraw each entry.")

(defun graphql-doc--history-push (name callback)
  "Add history entry with NAME and CALLBACK."
  (setq-local graphql-doc--history (cons `(,name . ,callback) graphql-doc--history)))

(defun graphql-doc-go-back ()
  "Go back to previous history entry."
  (interactive)
  (when (> (length graphql-doc--history) 1)
    (setq-local graphql-doc--history (cdr graphql-doc--history))
    (funcall (cdr (car graphql-doc--history)))))

(provide 'graphql-doc)

;;; graphql-doc.el ends here
