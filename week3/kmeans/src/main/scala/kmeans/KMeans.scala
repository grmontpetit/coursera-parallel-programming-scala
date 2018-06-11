package kmeans

import scala.annotation.tailrec
import scala.collection._
import scala.util.Random
import org.scalameter._
import common._

class KMeans {

  type Mean = Point

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to[mutable.ArrayBuffer]
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to[mutable.ArrayBuffer]
  }

  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    assert(means.nonEmpty)
    var minDistance = p.squareDistance(means.head)
    var closest = means.head
    var i = 1
    while (i < means.length) {
      val distance = p.squareDistance(means(i))
      if (distance < minDistance) {
        minDistance = distance
        closest = means(i)
      }
      i += 1
    }
    closest
  }

  /**
    * This method takes a generic sequence
    * of points and a generic sequence of means.
    * It returns a generic map collection,
    * which maps each mean to the sequence of
    * points in the corresponding cluster.
    */
  def classify(points: GenSeq[Point], means: GenSeq[Mean]): GenMap[Mean, GenSeq[Point]] = {
    if (points.isEmpty)
      means.map(m => (m, Nil)).toMap
    else
      points.groupBy(findClosest(_, means))
  }

  /**
    * Rewrote the findAverage method...
    * I did not like the other one.
    */
  def findAverage(oldMean: Mean, points: GenSeq[Point]): Point = {
    val sums = points.foldLeft((0.0,0.0,0.0)) { (acc, p) =>
      (acc._1 + p.x, acc._2 + p.y, acc._3 + p.z)
    }
    new Point(sums._1 / points.length, sums._2 / points.length, sums._3 / points.length)
  }

// Do not use vars.
//  def findAverage(oldMean: Point, points: GenSeq[Point]): Point = if (points.isEmpty) oldMean else {
//    var x = 0.0
//    var y = 0.0
//    var z = 0.0
//    points.seq.foreach { p =>
//      x += p.x
//      y += p.y
//      z += p.z
//    }
//    new Point(x / points.length, y / points.length, z / points.length)
//  }

  /**
    * Implement the method update, which takes the
    * map of classified points produced in the
    * previous step, and the sequence of previous means.
    * The method returns the new sequence of means:
    */
  def update(classified: GenMap[Mean, GenSeq[Point]], oldMeans: GenSeq[Mean]): GenSeq[Point] = {
    oldMeans.map(m => findAverage(m, classified(m)))
  }

  /**
    * Finally, you will implement convergence detection.
    * The convergence detection method takes a sequence
    * of old means and the sequence of updated means,
    * and returns a boolean indicating if the algorithm
    * converged or not. Given an eta parameter,
    * oldMeans and newMeans, it returns true if the
    * algorithm converged, and false otherwise.
    *
    * The algorithm converged if the square distance
    * between the old and the new mean is less than or
    * equal to eta, for all means.
    */
  def converged(eta: Double)(oldMeans: GenSeq[Mean], newMeans: GenSeq[Mean]): Boolean = {
    if (oldMeans.length != newMeans.length)
      throw new Error("oldMeans size is not equal to newMeans size !")
    else
      !oldMeans.zip(newMeans).map(s => s._1.squareDistance(s._2)).exists(_ > eta)
  }

  /**
    * We now have everything we need to run the K-means algorithm.
    * We only need to combine the previously defined methods
    * in the right way.
    *
    * The tail-recursive kMeans method takes a
    * sequence of points points, previously
    * computed sequence of means means, and the eta value.
    */
  @tailrec
  final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    val m = update(classify(points, means), means)
    if (!converged(eta)(means, m)) kMeans(points, m, eta)
    else m // your implementation need to be tail recursive
  }
}

/** Describes one point in three-dimensional space.
 *
 *  Note: deliberately uses reference equality.
 */
class Point(val x: Double, val y: Double, val z: Double) {
  private def square(v: Double): Double = v * v
  def squareDistance(that: Point): Double = {
    square(that.x - x)  + square(that.y - y) + square(that.z - z)
  }
  private def round(v: Double): Double = (v * 100).toInt / 100.0
  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"
}


object KMeansRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val kMeans = new KMeans()

    val numPoints = 500000
    val eta = 0.01
    val k = 32
    val points = kMeans.generatePoints(k, numPoints)
    val means = kMeans.initializeMeans(k, points)

    val seqtime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      val parPoints = points.par
      val parMeans = means.par
      kMeans.kMeans(parPoints, parMeans, eta)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}
