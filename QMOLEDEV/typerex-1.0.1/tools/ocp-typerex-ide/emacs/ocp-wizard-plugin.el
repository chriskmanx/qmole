(defun ocp-rename ()
  "Rename the current identifier"
  (interactive)
  (checked-string-command "rename"))


(defun ocp-rename-toplevel ()
  "Rename the toplevel module defined by the current source file"
  (interactive)
  (checked-string-command "rename-toplevel"))


(defun ocp-undo ()
  "Undo the last multiple-file refactoring action"
  (interactive)
  (checked-string-command "undo"))


(defun ocp-callback-test ()
  "testing callbacks"
  (interactive)
  (checked-string-command "callback-test"))


(defun ocp-goto-definition ()
  "Go to the definition referenced by the current ident"
  (interactive)
  (checked-string-command "goto-definition"))


(defun ocp-comment-definition ()
  "Show the comments associated with the current ident"
  (interactive)
  (checked-string-command "comment-definition"))


(defun ocp-cycle-definitions ()
  "Cycle between the alternative definitions of the current def."
  (interactive)
  (checked-string-command "cycle-definitions"))


(defun ocp-prune-lids ()
  "Simplify all references in the current file"
  (interactive)
  (checked-string-command "prune-lids"))


(defun ocp-eliminate-open ()
  "Eliminate an open statement by qualifying all references"
  (interactive)
  (checked-string-command "eliminate-open"))


(defun ocp-wizard-plugin ()
  (define-key (current-local-map) [(control o) (q)] 'ocp-eliminate-open)
  (define-key (current-local-map) [(control o) (p)] 'ocp-prune-lids)
  (define-key (current-local-map) [(control o) (a)] 'ocp-cycle-definitions)
  (define-key (current-local-map) [(control o) (c)] 'ocp-comment-definition)
  (define-key (current-local-map) [(control o) (d)] 'ocp-goto-definition)
  (define-key (current-local-map) [(control o) (control t)] 'ocp-callback-test)
  (define-key (current-local-map) [(control o) (u)] 'ocp-undo)
  (define-key (current-local-map) [(control o) (t) (r)] 'ocp-rename-toplevel)
  (define-key (current-local-map) [(control o) (r)] 'ocp-rename))
