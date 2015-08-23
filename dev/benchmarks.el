(require 'dash)


(defvar dash-benchmarks nil
  "List of all defined benchmarks, last declared first.")

(defmacro run-benchmark (num-tries &optional repetitions &rest forms)
  `(-let ((all-results nil))
     (--dotimes ,num-tries
       (garbage-collect)
       (!cons (benchmark-run-compiled ,repetitions ,@forms) all-results))
     all-results))

(defmacro defbenchmark (name details &optional repetitions &rest forms)
  (declare (indent 3))
  `(!cons (list (quote ,name) (quote ,details) (lambda (num-tries) (run-benchmark num-tries ,repetitions ,@forms)))
          dash-benchmarks))

(defun should-select-benchmark (name details selector)
  (cond ((eq selector t)
         t)
        ((eq selector nil)
         nil)
        ((stringp selector)
         (string-match selector (format "%s %s" name (or details ""))))
        ((and (consp selector) (eq (car selector) 'not))
         (not (should-select-benchmark name details (nth 1 selector))))
        ((and (consp selector) (eq (car selector) 'and))
         (--all? (should-select-benchmark name details it) (cdr selector)))
        ((and (consp selector) (eq (car selector) 'or))
         (--any? (should-select-benchmark name details it) (cdr selector)))
        (t
         (error "Unhandled selector '%s'" selector))))

(defun select-benchmarks (selector)
  (nreverse (--filter (-let (((name details _) it))
                        (should-select-benchmark name details selector))
                      dash-benchmarks)))

(defun run-benchmarks-and-exit (&optional num-tries selector)
  (-let* ((benchmarks     (select-benchmarks (or selector t)))
          (num-benchmarks (length benchmarks)))
    (unless num-tries
      (setq num-tries 10))
    (--each benchmarks
      (-let (((name details runner) it))
        (-let* ((all-results (funcall runner num-tries))
                (best-result (--min-by (< (- (nth 0 other) (nth 2 other)) (- (nth 0 it) (nth 2 it))) all-results))
                (total-time  (- (-sum (--map (nth 0 it) all-results )) (-sum (--map (nth 2 it) all-results )))))
          (message "%3d/%d %-30sbest of %d tries: %.3f s%s; average: %.3f s"
                   (1+ it-index) num-benchmarks
                   (format "%s %s" name (or details ""))
                   num-tries (- (nth 0 best-result) (nth 2 best-result))
                   (if (= (nth 1 best-result) 0)
                       ""
                     (format " (%d GC runs)" (nth 1 best-result)))
                   (/ total-time num-tries)))))))



(defvar benchmark--1000-integers (--map-indexed it-index (-repeat 1000 nil)))

(defbenchmark --each-while <0
              1000000
  (--each-while benchmark--1000-integers (< it 0)))

(defbenchmark --each-while <500
              10000
  (--each-while benchmark--1000-integers (< it 500)))
