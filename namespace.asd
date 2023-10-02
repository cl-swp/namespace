(asdf:defsystem :namespace
  :name "namespace"
  :author "Seiji Koide <koide@ontolonomy.co.jp>"
  :version "0.9.0"
  :maintainer "Seiji Koide <koide@ontolonomy.co.jp>"
  :licence "MIT"
  :description "Library for Name Space on Semantic Webs"
  :depends-on ("iri")
  :serial t
  :components ((:depends-on (:file "../utilities/utilities"))
               (:depends-on (:file "../utilities/ncutils"))
               (:file "namespace")))