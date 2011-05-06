package org.everpeace.util

import scala.math.ceil
import scala.math.log
import scala.math.min
import scala.math.abs
import scala.math.exp
import scala.math.pow
import java.security.MessageDigest
import java.nio.charset.Charset

import scalaz._
import Scalaz._
import CustomScalaz._

/**
 * Counting Bloom Filter
 *
 * @constructor create a counting bloom filter.
 * @param size is filter size (i.e. number of bits).
 * @param expectedElements is the number of elements to be contained in the filter.
 * @param k is the number of hash functions.
 * @author everpeace _at_ gmail _dot_ com
 * @date 11/05/03
 */
class CountingBloomFilter(val size: Int, val expectedElements: Int, val k: Int) {

  require(size > 0, "filter size must be positive.");
  require(expectedElements > 0, " expected number of elements must be positive.")
  require(k > 0, "number of hash functions must be positive")

  private[this] var filter = new Array[Int](size)
  private[this] var numberOfContains: Long = 0
  private[this] val hashFunction = MessageDigest.getInstance("MD5")

  /**
   * add an element from the filter.
   * @param elm is an elements you want to add.
   */
  def add[E <% Array[Byte]](elm: E): Unit = {
    this.synchronized({
      createHashes(implicitly(elm), k) |>| ((c: Int) => filter(c) += 1)
      numberOfContains += 1
    })
  }

  /**
   * add all given elements from the filter.
   * @param elms is a set of elements you want to add.
   */
  def addAll[E <% Array[Byte]](elms: Set[E]): Unit = elms |>| (add(_))

  /**
   * discard an element from the filter.
   * @param elm is an elements you want to discard.
   */
  def discard[E <% Array[Byte]](elm: E): Unit = {
    this.synchronized({
      createHashes(implicitly(elm), k) |>| ((c: Int) => {
        filter(c) -= 1
        if (filter(c) < 0) filter(c) = 0
      })
      numberOfContains -= 1
    })
  }

  /**
   * discard all given elements from the filter.
   * @param elms is a set of elements you want to discard.
   */
  def discardAll[E <% Array[Byte]](elms: Set[E]): Unit = elms |>| (discard(_))

  /**
   * check whether the filter contains the given elements.
   * @param elm is an elements you want to check.
   * @return true: the elements contained, false: otherwise.
   */
  def contains[E <% Array[Byte]](elm: E): Boolean = {
    ∀(createHashes(implicitly(elm), k) ∘ (filter(_) > 0))(_ == true)
  }

  /**
   * check whether the filter contains all the given elements.
   * @param elms is a set of elements you want to check.
   * @return true: all the elements contained, false: otherwise.
   */
  def containsAll[E <% Array[Byte]](elms: Set[E]): Boolean = {
    ∀(elms ∘ (contains(_)))(_ == true)
  }

  /**
   * @return expected false positive probability of the filter (based on expectedElements).
   */
  // (1 - e^(-k * n / m)) ^ k
  def expectedFalsePositiveProbability: Double = pow(1 - exp(-1 * k * expectedElements.toDouble / size), k)

  /**
   * @return the number of elements contained in the filter
   */
  def counts: Long = numberOfContains;

  /**
   * reset the filter. (discard all elements)
   */
  def reset = {
    this.synchronized({
      filter = new Array[Int](size)
      numberOfContains = 0
    })
  }

  /**
   * create hash values (indices for increment counters)
   * each index is generated from 4-byte data(Int) split from hash value (byte array).
   */
  private def createHashes(data: Array[Byte], hashes: Int): List[Int] = {
    val result = new Array[Int](hashes)

    var remainNum: Int = hashes
    var _k: Int = 0;
    var salt: Int = 0;

    while (_k < hashes) {
      var hashValue: Array[Byte] = new Array[Byte](hashes)

      hashFunction.synchronized({
        import CountingBloomFilter.IntToBytes
        hashFunction.update(IntToBytes(salt));
        salt += 1
        hashValue = hashFunction.digest(data);
      })

      var createdNum: Int = 0
      for (i: Int <- 0 to min((hashValue.length / 4) - 1, remainNum) - 1) {
        result(_k) = BytesToInt(hashValue, (i * 4))
        _k += 1
        createdNum += 1
      }
      remainNum -= createdNum
    }
    val ret: List[Int] = result.toList ∘ (Int2Index(_))
    return ret
  }

  /**
   * alias for add
   */
  def <++[E <% Array[Byte]](e: E) = add(e)

  /**
   * alias for discard
   */
  def -->[E <% Array[Byte]](e: E) = discard(e)

  /**
   * alias in contains
   */
  def ∋[E <% Array[Byte]](e: E) = contains(e)

  /**
   * calculate Int value which is represented by bytes(i..i+3)
   * @param bytes is a byte array.
   * @param i is an start index.
   * @return Int value represent by bytes(i..i+3)
   */
  private def BytesToInt(bytes: Array[Byte], i: Int): Int = {
    var h = 0;
    for (j <- i to i + 3) {
      h = (h << 8) | (bytes(j).asInstanceOf[Int] & 0xFF)
    }
    return h
  }

  /**
   * round Int value to [0..size)
   */
  private def Int2Index(i: Int): Int = abs(i % size)

}

/**
 * companion object for CountingBloomFilter class
 */
object CountingBloomFilter {
  /**String to Array[Byte] */
  implicit val StringToBytes: String => Array[Byte] = _.toString.getBytes(Charset.forName("UTF-8"))
  /**Byte to Array[Byte] */
  implicit val ByteToBytes: Byte => Array[Byte] = (data: Byte) => (for (s <- 0 to 0) yield (data >> (s * 8) & 0xFF).byteValue).toArray
  /**Short to Array[Byte] */
  implicit val ShortToBytes: Short => Array[Byte] = (data: Short) => (for (s <- 0 to 1) yield (data >> (s * 8) & 0xFF).byteValue).toArray
  /**Int to Array[Byte] */
  implicit val IntToBytes: Int => Array[Byte] = (data: Int) => (for (s: Int <- 0 to 3) yield (data >> (s * 8) & 0xFF).byteValue).toArray
  /**Long to Array[Byte] */
  implicit val LongToBytes: Long => Array[Byte] = (data: Long) => (for (s: Int <- 0 to 7) yield (data >> (s * 8) & 0xFF).byteValue).toArray
  /**Float to Array[Byte] */
  implicit val FloatToBytes: Float => Array[Byte] = (data: Float) => IntToBytes(java.lang.Float.floatToIntBits(data))
  /**Double to to Array[Byte] */
  implicit val DoubleToBytes: Double => Array[Byte] = (data: Double) => LongToBytes(java.lang.Double.doubleToLongBits(data))

  implicit def VtoPimpedVal[V <% Array[Byte]](v: V) = new PimpedValForCountingBloomFilter(v)

  /**
   * factory for default constructor
   */
  def apply(size: Int, expectedElements: Int, k: Int) = {
    new CountingBloomFilter(size, expectedElements, k)
  }

  /**
   * factory with max false positive probability.
   * @param maxFalsePositiveProbability is max false positive probability
   * @param expectedElements is the number of elements to be contained in the filter.
   */
  def apply(maxFalsePositiveProbability: Double, expectedElements: Int) = {
    require(0 < maxFalsePositiveProbability && maxFalsePositiveProbability < 1, "max false positive probability must be in (0,1).")
    val k = ceil(-(log(maxFalsePositiveProbability) / log(2))) // k = ceil(-log_2(false positive prob.)))
    val n = k * expectedElements / log(2.0) // n = k * expectedElements / log(2)
    new CountingBloomFilter(n.asInstanceOf[Int], expectedElements, k.asInstanceOf[Int])
  }

  /**
   * factory with filter size.
   * @param size is filter size (i.e. number of bits).
   * @param expectedElements is the number of elements to be contained in the filter.
   */
  def apply(size: Int, expectedElements: Int) = {
    // k = (m / n) * ln 2 : this value minimizes false positive probability.
    val k = ceil(size.asInstanceOf[Double] / expectedElements * log(2.0d))
    new CountingBloomFilter(size, expectedElements, k.asInstanceOf[Int])
  }

}

/**
 * Pimp for Counting Bloom Filter.
 */
class PimpedValForCountingBloomFilter[V <% Array[Byte]](val v: V) {

  /**
   * add to a given filter
   */
  def addTo(filter: CountingBloomFilter) = filter.add(v)

  /**
   * alias for addTo
   */
  def ++>(filter: CountingBloomFilter) = addTo(filter)

  /**
   * discard from a given filter
   */
  def discardFrom(filter: CountingBloomFilter) = filter.discard(v)

  /**
   * alias for discardFrom
   */
  def <--(filter: CountingBloomFilter) = discardFrom(filter)

  /**
   * check contains in a given filter
   */
  def containsIn(filter: CountingBloomFilter) = filter.contains(v)

  /**
   * alias for containsIn
   */
  def ∈(filter: CountingBloomFilter) = containsIn(filter)
}

/**
 *  Scalaz Custom Object
 */
object CustomScalaz{

  def ∀[M[_],A](v:M[A])(p: A => Boolean)(implicit cnv:M[A]=>MA[M,A],r: Foldable[M])= implicitly[MA[M,A]](v).all(p)
  def ♯[M[_],A](v:M[A])(implicit cnv:M[A]=>MA[M,A],r: Foldable[M]): Int = implicitly[MA[M,A]](v).count
}