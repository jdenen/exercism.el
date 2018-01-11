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
;;; exercism-test.el ends here
