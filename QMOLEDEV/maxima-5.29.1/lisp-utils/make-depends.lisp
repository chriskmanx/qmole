(in-package :mk)

(defvar *oos-dependency-stream* t
  "Output stream for list-dependencies-operation")

(defvar *oos-dependency-target* "target"
  "Output target for list-dependencies-operation")

(defun list-dependencies-operation (component force)
  (format *oos-dependency-stream* "~A : ~A.~A~%"
	  *oos-dependency-target*
	  (component-source-pathname component)
	  (component-source-extension component)))

(component-operation :list-dependencies 'list-dependencies-operation)

(defun create-dependency-file (target file)
  (with-open-file (dependency-file file :direction :output :if-exists :supersede)
    (setq *oos-dependency-target* target)
    (setq *oos-dependency-stream* dependency-file)
    (operate-on-system 'maxima :list-dependencies)
    (format dependency-file "~%")))
