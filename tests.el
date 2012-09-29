(require 'ert)
(require 'bang)

(defun even? (num) (= 0 (% num 2)))
(defun square (num) (* num num))

(ert-deftest map ()
  "`!map' returns a new list with the results of calling the function on each element."
  (should (equal (!map (lambda (num) (* num num)) '(1 2 3 4)) '(1 4 9 16)))
  (should (equal (!map 'square '(1 2 3 4)) '(1 4 9 16)))
  (should (equal (!!map (* it it) '(1 2 3 4)) '(1 4 9 16)))
  )

(ert-deftest reduce-from ()
  "`!reduce-from' takes a list and an initial value, and applies the function over them to create one result"
  (should (equal (!reduce '+ '()) 0))
  (should (equal (!reduce '+ '(1)) 1))
  (should (equal (!reduce '+ '(1 2)) 3))
  (should (equal (!reduce-from '+ 7 '()) 7))
  (should (equal (!reduce-from '+ 7 '(1)) 8))
  (should (equal (!reduce-from '+ 7 '(1 2)) 10))
  (should (equal (!!reduce-from (+ acc it) 7 '(1 2 3)) 13))
  )

(ert-deftest reduce ()
  "`!reduce' takes a list and applies the function over the elements to create one result"
  (should (equal (!reduce (lambda (memo item) (format "%s-%s" memo item)) '(1 2 3)) "1-2-3"))
  (should (equal (!!reduce (format "%s-%s" acc it) '(1 2 3)) "1-2-3"))
  (should (equal (!!reduce (format "%s-%s" acc it) '()) "nil-nil"))
  )

(ert-deftest filter ()
  "`!filter' returns a new list of only those elements where the predicate was non-nil."
  (should (equal (!filter (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) '(2 4)))
  (should (equal (!filter 'even? '(1 2 3 4)) '(2 4)))
  (should (equal (!!filter (= 0 (% it 2)) '(1 2 3 4)) '(2 4)))
  )

(ert-deftest remove ()
  "`!remove' returns a new list of only those elements where the predicate was nil."
  (should (equal (!remove (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) '(1 3)))
  (should (equal (!remove 'even? '(1 2 3 4)) '(1 3)))
  (should (equal (!!remove (= 0 (% it 2)) '(1 2 3 4)) '(1 3)))
  )

(ert-deftest concat ()
  "`!concat' returns the concatenation of the elements in the supplied lists"
  (should (equal (!concat) nil))
  (should (equal (!concat '(1)) '(1)))
  (should (equal (!concat '(1) '(2)) '(1 2)))
  (should (equal (!concat '(1) '(2 3) '(4)) '(1 2 3 4)))
  )

(ert-deftest mapcat ()
  "`!mapcat' applies the function to all elements of the list and then concatenates the result"
  (should (equal (!mapcat 'list '(1 2 3)) '(1 2 3)))
  (should (equal (!mapcat (lambda (item) (list 0 item)) '(1 2 3)) '(0 1 0 2 0 3)))
  (should (equal (!!mapcat (list 0 it) '(1 2 3)) '(0 1 0 2 0 3)))
  )

(ert-deftest partial ()
  "`!partial' returns a function like fn where the first arguments are filled in"
  (should (equal (funcall (!partial '+ 5) 3) 8))
  (should (equal (funcall (!partial '+ 5 2) 3) 10))
  )

(ert-deftest difference ()
  "`!difference' returns a new list of only elements in list1 that are not in list2."
  (should (equal (!difference '() '()) '()))
  (should (equal (!difference '(1 2 3) '(4 5 6)) '(1 2 3)))
  (should (equal (!difference '(1 2 3 4) '(3 4 5 6)) '(1 2)))
  )

(ert-deftest intersection ()
  "`!intersection' returns a new list of only elements that are in both given lists."
  (should (equal (!intersection '() '()) '()))
  (should (equal (!intersection '(1 2 3) '(4 5 6)) '()))
  (should (equal (!intersection '(1 2 3 4) '(3 4 5 6)) '(3 4)))
  )

(ert-deftest uniq ()
  "`!uniq' returns a new list of only unique elements."
  (should (equal (!uniq '()) '()))
  (should (equal (!uniq '(1 2 2 4)) '(1 2 4)))
  )

(ert-deftest contains? ()
  "`!contains?' returns t if the list contains the element."
  (should (!contains? '(1 2 3) 1))
  (should (!contains? '(1 2 3) 2))
  (should (not (!contains? '() '())))
  (should (not (!contains? '() 1)))
  (should (not (!contains? '(1 2 4) 3)))
  )
