package org.everpeace.util

/**
 * Sample for CountingBloomFilter
 * @author everpeace _at_ gmail _dot_ com
 * @date 11/05/03
 */

object CountingBloomFilterSample {
  /**
   * main.
   * @param args :the first argument is max false positive probability to set.
   */
  def main(args: Array[String]): Unit = {
    Scenario(args(0).toDouble)
  }

  /**
   * run scenario.
   */
  private def Scenario(p: Double) = {
    import org.everpeace.util.CountingBloomFilter._
    System.out.println("=========================================================================================================================================")
    System.out.println("Test Senario:")
    System.out.println("0.initialize a CountingBloomFilter with max false positive probability is %1.7f and expected number of item is 26^4(about 4.5*10^5)" format p)
    System.out.println("1.add all strings such that [a-z]^4 to the filter")
    System.out.println("2.discard all strings such that a[a-z]^3 from the filter")
    System.out.println("3.check that the filter contains each filter. (outputs only false positive results)")
    System.out.println("-----------------------------------------------------------------------------------------------------------------------------------------")

    val filter = CountingBloomFilter(p, 26 * 26 * 26 * 26)
    System.out.println("[FilterInfo] filter-bit-size:%d, number of hash:%d, max false positive prob.: %1.7f".format(filter.size, filter.k, p))

    var addTime: Long = 0
    var addTimes: Long = 0
    for (a <- 'a' to 'z';
         b <- 'a' to 'z';
         c <- 'a' to 'z';
         d <- 'a' to 'z') {
      val str = a.toString ++ b.toString ++ c.toString ++ d.toString
      addTime += executeTime(filter.add(str))
      addTimes += 1
    }
    var discardTime: Long = 0
    var discardTimes: Long = 0
    for (b <- 'a' to 'z';
         c <- 'a' to 'z';
         d <- 'a' to 'z') {
      val str = 'a'.toString ++ b.toString ++ c.toString ++ d.toString
      discardTime += executeTime(filter.discard(str))
      discardTimes += 1
    }

    var foundTimes: Long = 0
    var checkTime: Long = 0
    var checkTimes: Long = 0
    System.out.println("[Detected false positive results]")
    for (a <- 'a' to 'z';
         b <- 'a' to 'z') {
      var localFound = false;
      for (c <- 'a' to 'z';
           d <- 'a' to 'z') {
        val str = a.toString ++ b.toString ++ c.toString ++ d.toString
        var result: Boolean = false
        checkTime += executeTime(result = filter.contains(str))
        checkTimes += 1
        if (a == 'a' && result) {
          System.out.print("%3s:%4s ".format(str, true))
          localFound = true
          foundTimes += 1
        }
        if (c == 'z' && d == 'z' && localFound) System.out.println()
      }
    }
    if (foundTimes == 0) {
      System.out.println("No false positive results found.")
    } else {
      System.out.println("[mesured false positive prob.] %1.7f (%d false positives found in %d trials)".format(foundTimes.toDouble / checkTimes, foundTimes, checkTimes))
    }
    System.out.println("[average time for     add] %2.3f[ms] (average for %d times trials)".format(addTime.toDouble / addTimes, addTimes))
    System.out.println("[average time for discard] %2.3f[ms] (average for %d times trials)".format(discardTime.toDouble / discardTimes, discardTimes))
    System.out.println("[average time for   check] %2.3f[ms] (average for %d times trials)".format(checkTime.toDouble / checkTimes, checkTimes))
    System.out.println("=========================================================================================================================================")
  }

  /**
   * measure execution time for proc.
   * @param proc is process.
   */
  private def executeTime(proc: => Unit): Long = {
    val start = System.currentTimeMillis
    proc
    return System.currentTimeMillis - start
  }
}