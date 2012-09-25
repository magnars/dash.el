(require 'ert)
(require 'bang)

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
