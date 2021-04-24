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

(provide 'graphql-doc)

;;; graphql-doc.el ends here
