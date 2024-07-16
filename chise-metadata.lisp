(in-package :chise)

(defun define-bibliography (bib-spec)
  (let ((id (cdr (assoc '=chise-bib-id bib-spec))))
    (concord:define-object :bibliography bib-spec :id id)))
