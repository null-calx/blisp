(defsystem "readelf"
  :description "readelf: an ELF parser"
  :version "0.0.1"
  :author "calx"
  :components ((:file "packages")
	       (:file "parser")
	       (:file "readelf")))
