package example

object Lists:

  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   * ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   * solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
  def sum(xs: List[Int]): Int = {
    //    if(xs == null || xs.length == 0){
    //      throw new Exception()
    //    }
    sum_helper(xs, 0, xs.length);
  }


  def sum_helper(xs: List[Int], start: Int, end: Int): Int = {
    if (start >= end) {
      0;
    }
    else if (start + 1 == end) {
      xs(start);
    }
    else {
      var mid: Int = (end - start) / 2 + start;
      sum_helper(xs, start, mid) + sum_helper(xs, mid, end);
    }
  }

  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
  def max(xs: List[Int]): Int = {
    max_helper(xs, 0, xs.length);
  }

  def max_helper(xs: List[Int], start: Int, end: Int): Int = {
    if (start >= end) {
      throw new java.util.NoSuchElementException("Empty List xs");
    }
    else if (start + 1 == end) {
      xs(start);
    }
    else {
      var mid: Int = (end - start) / 2 + start;
      var first: Int = max_helper(xs, start, mid);
      var second: Int = max_helper(xs, mid, end);

      if (first > second) {
        first;
      }
      else {
        second;
      }
    }
  }