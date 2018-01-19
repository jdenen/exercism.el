;;; exercism-test.el --- Tests for exercism.el
(ert-deftest submit-buffer ()
  "Submits current file as solution."
  (with-mock
    (mock (buffer-file-name) => "file-name.ext")
    (mock (exercism--run-command "submit file-name.ext"))
    (call-interactively 'exercism-submit-buffer)))

(ert-deftest fetch ()
  "Lets users choose what to fetch from languages already retrieved."
  (let ((exercism-json-file "/path/to/exercism.json"))
    (with-mock
      (mock (json-read-file "/path/to/exercism.json") => '((dir . "/path/to/exercism/dir/")))
      (mock (directory-files "/path/to/exercism/dir/" nil "[^\.]+$") => '("one" "two"))
      (mock (completing-read "Fetch for: " '("one" "two")) => "two")
      (mock (exercism--run-command "fetch two"))
      (call-interactively 'exercism-fetch))))

(ert-deftest run-command ()
  "Executes an exercism CMD and writes its response to a new buffer."
  (with-mock
    (mock (start-process-shell-command "exercism-cmd"
                                       "*exercism-cmd*"
                                       "exercism cmd arg1 arg2"))
    (exercism--run-command "cmd arg1 arg2")))

;;; exercism-test.el ends here
