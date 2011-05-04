package org.everpeace.util

import org.scalacheck.Gen

/**
 * Sample for CountingBloomFilter
 * @author everpeace _at_ gmail _dot_ com
 * @date 11/05/03
 */

object CountingBloomFilterSample {

  /**
   * main.
   * @param args <max false positive prob.> <number of trials>
   */
  def main(args: Array[String]): Unit = {
    Scenario(args(0).toDouble, args(1).toInt)
  }

  /**
   * run scenario.
   */
  private def Scenario(p: Double, n: Int) = {
    import org.everpeace.util.CountingBloomFilter._
    System.out.println("=========================================================================================================================================")
    System.out.println("Sample Senario:")
    System.out.println("0.initialize a CountingBloomFilter with max false positive probability is %1.7f and expected number of items is %d" format (p, n))
    System.out.println("1.add distinct random %d strings the filter (twice of your input)" format 2 * n)
    System.out.println("2.discard %d strings of added (your input)" format n)
    System.out.println("3.check that the filter contains each filter. (outputs only false positive results)")
    System.out.println("-----------------------------------------------------------------------------------------------------------------------------------------")

    // initialize a filter
    val filter = CountingBloomFilter(p, n)
    System.out.println("[FilterInfo] filter-bit-size:%d, number of hash:%d, max false positive prob.: %1.7f".format(filter.size, filter.k, p))

    // add 2n distinct random strings
    System.out.print("[Adding %d distinct random strings]" format 2 * n)
    var addTime: Long = 0
    var addTimes: Long = 0
    val added = genStrings(2 * n)
    for (str <- added) {
      addTime += executeTime(filter.add(str))
      addTimes += 1
      if (addTimes % (2 * n / 10) == 0) System.out.print('.')
    }
    System.out.println(" done.")

    // discard first n distinct strings from added strings
    System.out.print("[Discarding %d strings]" format n)
    var discardTime: Long = 0
    var discardTimes: Long = 0
    val discarded = added.slice(0, n) // first n strings
    for (str <- discarded) {
      discardTime += executeTime(filter.discard(str))
      discardTimes += 1
      if (discardTimes % (n / 10) == 0) System.out.print('.')
    }
    System.out.println(" done.")

    // measure false positive prob.
    // check whether the filter says discarded strings contain.(false positive result)
    var foundTimes: Long = 0
    var checkTime: Long = 0
    val checkTimes: Int = n
    System.out.println("[Detected false positive results]")
    for (str <- discarded) {
      var result: Boolean = false
      checkTime += executeTime(result = filter.contains(str))
      if (result) {
        System.out.print("%6s ".format(str, true))
        foundTimes += 1
        if (foundTimes % 10 == 0) System.out.println
      }
    }
    if (foundTimes % 10 != 0) System.out.println
    if (foundTimes == 0) {
      System.out.println("  No false positive results found.")
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

  import org.scalacheck.Gen._

  // generator for string with length l
  private def str(i: Int) = for (cs <- listOfN(i, Gen.alphaNumChar)) yield cs.mkString

  // generator for AlphaNum^[1..6]
  private def sample: Gen[String] = Gen.frequency((1, str(1)), (2, str(2)), (3, str(3)), (4, str(4)), (5, str(4)), (6, str(4)))

  // generate num distinct random strings
  private def genStrings(num: Int): List[String] = {
    var added: Set[String] = Set.empty
    var numAdded = 0
    while (numAdded < num) {
      sample.sample match {
        case Some(s) => {
          if (!added.contains(s)) {
            added = added + s
            numAdded += 1
          }
        }
        case _ => {}
      }
    }
    added.toList
  }
}