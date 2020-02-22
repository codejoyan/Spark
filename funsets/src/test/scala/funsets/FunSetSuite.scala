package funsets

import org.junit._
import org.junit.Assert.assertEquals

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains overlapping elements of each set`: Unit = {
    new TestSets {
      val s = intersect((x: Int) => x >= 5 && x <= 10, (x: Int) => x > 9 && x <= 30)
      assert(!contains(s, 9), "Intersect 9")
      assert(contains(s, 10), "Intersect 10")
      assert(!contains(s, 21), "Intersect 21")
    }
  }

  @Test def `diff contains elements in set S but not in set T`: Unit = {
    new TestSets {
      val s = diff((x: Int) => x >= 5 && x <= 10, (x: Int) => x > 9 && x <= 30)
      assert(contains(s, 9), "Intersect 9")
      assert(!contains(s, 10), "Intersect 10")
      assert(contains(s, 5), "Intersect 5")
    }
  }

  @Test def `filter returns true that satisfies the predicate`: Unit = {
    new TestSets {
      val s = filter((x: Int) => x >= 5 && x <= 100, (x: Int) => x > 40 && x <= 45)
      assert(!contains(s, 99), "Intersect 99")
      assert(contains(s, 41), "Intersect 41")
      assert(contains(s, 45), "Intersect 45")
      assert(!contains(s, 46), "Intersect 46")
    }
  }

  @Test def `Returns whether all bounded integers within s satisfy p`: Unit = {
    new TestSets {
      assertEquals(forall((x: Int) => x >= 1 && x <= 100, _ % 2 == 0), false)
      assertEquals(forall((x: Int) => x >= 1 && x <= 100, _ > 0), true)
    }
  }

  @Test def `Returns whether there exists a bounded integer within s`: Unit = {
    new TestSets {
      assertEquals(exists((x: Int) => x >= 1 && x <= 100, _ % 2 == 0), true)
      assertEquals(exists((x: Int) => x >= 1 && x <= 100, _ < 0), false)
    }
  }
  
    @Test def `Returns a set transformed by applying f to each element of s`: Unit = {
    new TestSets {
      val m = map((x: Int) => x >= 5 && x <= 8, _ * 2)
      assert(contains(m, 14), "map 14")
      assert(contains(m, 12), "map 12")
      assert(!contains(m, 8), "map 8")
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}