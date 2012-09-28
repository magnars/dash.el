(require 'ert)
(require 'bang)

(defun even? (num) (= 0 (% num 2)))
(defun square (num) (* num num))

(ert-deftest filter ()
  "`!filter' returns a new list of only those elements where the predicate was non-nil."
  (should (equal (!filter (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) '(2 4)))
  (should (equal (!filter (= 0 (% it 2)) '(1 2 3 4)) '(2 4)))
  (should (equal (!filter even? '(1 2 3 4)) '(2 4))))

(ert-deftest map ()
  "`!map' returns a new list with the results of calling the function on each element."
  (should (equal (!map (lambda (num) (* num num)) '(1 2 3 4)) '(1 4 9 16)))
  (should (equal (!map (* it it) '(1 2 3 4)) '(1 4 9 16)))
  (should (equal (!map square '(1 2 3 4)) '(1 4 9 16))))

(ert-deftest reduce ()
  "`!reduce' takes a list and applies the function over them to create one result"
  (should (equal (!reduce + '()) 0))
  (should (equal (!reduce + '(1)) 1))
  (should (equal (!reduce + '(1 2)) 3))
  (should (equal (!reduce-from + 7 '()) 7))
  (should (equal (!reduce-from + 7 '(1)) 8))
  (should (equal (!reduce-from + 7 '(1 2)) 10))

  (should (equal (!reduce (lambda (memo item) (format "%s-%s" memo item)) '(1 2 3)) "1-2-3"))
  (should (equal (!reduce (format "%s-%s" acc it) '(1 2 3)) "1-2-3"))
  (should (equal (!reduce (format "%s-%s" acc it) '()) "nil-nil"))
)

(ert-deftest difference ()
  "`!difference' returns a new list of only elements in list1 that are not in list2."
  (should (equal (!difference '() '()) '()))
  (should (equal (!difference '(1 2 3) '(4 5 6)) '(1 2 3)))
  (should (equal (!difference '(1 2 3 4) '(3 4 5 6)) '(1 2))))

(ert-deftest intersection ()
  "`!intersection' returns a new list of only elements that are in both given lists."
  (should (equal (!intersection '() '()) '()))
  (should (equal (!intersection '(1 2 3) '(4 5 6)) '()))
  (should (equal (!intersection '(1 2 3 4) '(3 4 5 6)) '(3 4))))

(ert-deftest uniq ()
  "`!uniq' returns a new list of only unique elements."
  (should (equal (!uniq '()) '()))
  (should (equal (!uniq '(1 2 2 4)) '(1 2 4))))

(ert-deftest contains-p ()
  "`!contains-p' returns t if the list contains the element."
  (should (!contains-p '(1 2 3) 1))
  (should (!contains-p '(1 2 3) 2))
  (should (not (!contains-p '() '())))
  (should (not (!contains-p '() 1)))
  (should (not (!contains-p '(1 2 4) 3))))
