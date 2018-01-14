;;; exercism-test.el --- Tests for exercism.el
(ert-deftest submit-buffer-success ()
  "Messages success after submitting current buffer file as solution."
  (with-temp-buffer
    (let ((test-buffer (current-buffer)))
        (with-mock
          (mock (current-buffer) => test-buffer)
          (mock (buffer-file-name) => "/path/to/success.py")
          (mock (start-process-shell-command "exercism-submit" test-buffer "exercism submit /path/to/success.py")
                => (insert "Process exercism-submit finished"))
          (mock (message "Submission accepted."))
          (call-interactively 'exercism-submit-buffer)))))

(ert-deftest submit-buffer-failure ()
  "Opens failure buffer after failing to submit current buffer file as solution."
  (with-temp-buffer
    (let ((test-buffer (current-buffer)))
      (with-mock
        (mock (current-buffer) => test-buffer)
        (mock (buffer-file-name) => "/path/to/failure.py")
        (mock (start-process-shell-command "exercism-submit" test-buffer "exercism submit /path/to/failure.py")
              => (insert "Process exercism-submit exited abnormally with code 1"))
        (mock (switch-to-buffer-other-window test-buffer))
        (call-interactively 'exercism-submit-buffer)))))

(ert-deftest fetch-from-list ()
  (let ((exercism-json-file "/path/to/exercism.json"))
    (with-mock
      (mock (json-read-file "/path/to/exercism.json") => '((dir . "/path/to/exercism/dir/")))
      (mock (directory-files "/path/to/exercism/dir/" nil "[^\.]+$") => '("one" "two"))
      (mock (completing-read "Fetch for: " '("one" "two")) => "two")
      (mock (exercism--run-command "fetch two"))
      (call-interactively 'exercism-fetch))))

(ert-deftest run-command-success-callback ()
  "Runs SUCCESS callback if exercism command is successful."
  (with-temp-buffer
    (let ((test-buffer (current-buffer)))
      (with-mock
        (mock (current-buffer) => test-buffer)
        (mock (start-process-shell-command "exercism-foo" test-buffer "exercism foo arg1 arg2")
              => (insert "Process exercism-foo finished"))
        (should (eq t
                    (exercism--run-command "foo arg1 arg2"
                                           :success (lambda (x) (string= "Process exercism-foo finished" x))
                                           :failure (lambda (_) nil))))))))

(ert-deftest run-command-failure-callback ()
  "Runs FAILURE callback function if exercism command fails."
  (with-temp-buffer
    (let ((test-buffer (current-buffer)))
      (with-mock
        (mock (current-buffer) => test-buffer)
        (mock (start-process-shell-command "exercism-foo" test-buffer "exercism foo arg1 arg2")
              => (insert "Process exercism-foo exited abnormally with code 1"))
        (should (eq t
                    (exercism--run-command "foo arg1 arg2"
                                           :success (lambda (_) nil)
                                           :failure (lambda (x) (string= "Process exercism-foo exited abnormally with code 1" x)))))))))

(ert-deftest run-command-no-callback ()
  "Runs and command and switches to output buffer with no callbacks given."
  (with-temp-buffer
    (let ((test-buffer (current-buffer)))
      (with-mock
        (mock (current-buffer) => test-buffer)
        (mock (start-process-shell-command "exercism-no-callback" test-buffer "exercism no-callback arg1 arg2"))
        (mock (switch-to-buffer-other-window test-buffer))
        (exercism--run-command "no-callback arg1 arg2")))))
;;; exercism-test.el ends here
