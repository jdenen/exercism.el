;;; exercism-test.el --- Tests for exercism.el
(ert-deftest submit-buffer ()
  "Submits current buffer file as a solution."
  (with-mock
    (mock (buffer-file-name) => "/path/to/foo.py")
    (mock (exercism--run-command "submit /path/to/foo.py"
                                 :success 'exercism--run-callback
                                 :failure 'exercism--run-callback))
    (exercism-submit-buffer)))

(ert-deftest run-callback-message ()
  "Messages user with RESULT status and details BUFFER."
  (with-mock
    (mock (message "SUCCESS: See *exercism-test-foo* buffer for details."))
    (exercism--run-callback 'success "*exercism-test-foo*")))

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
                                           :success (lambda (x y) (and (eq 'success x)
                                                                       (string= "Process exercism-foo finished" y)))
                                           :failure (lambda (x y) nil))))))))

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
                                           :success (lambda (x y) nil)
                                           :failure (lambda (x y) (and (eq 'failure x)
                                                                       (string= "Process exercism-foo exited abnormally with code 1" y))))))))))
;;; exercism-test.el ends here
