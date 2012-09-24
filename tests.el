(require 'ert)
(require 'bang)

(ert-deftest intersection ()
  "`!intersection' returns a new list of only elements that are in both given lists."
  (should (equal (!intersection '(1 2 3 4) '(3 4 5 6)) '(3 4))))
