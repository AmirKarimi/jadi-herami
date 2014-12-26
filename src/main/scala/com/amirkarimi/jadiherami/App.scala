package com.amirkarimi.jadiherami

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.annotation._

object App {

  def main(args: Array[String]): Unit = {
    val benefitIternationCount = getBenefitIterationCount()
    val benefitGap = 4
    
    val iterationMap = (0 to 21) map { i =>
      (i -> (iterate(i) + 1))
    }

    iterationMap map { case (index, totalCount) =>
      val benefitedIndex = index - (benefitIternationCount + benefitGap)
      val benefitedCount = iterationMap.find(_._1 == benefitedIndex).map(_._2).getOrElse(0)
      
      println(s"Month: ${"%02d".format(index)} - Population: ${"%,14d".format(totalCount)} - Benefited: ${"%,14d".format(benefitedCount)}")
    }
  }
  
  /*
   * Returns the iteration count which is needed to reach the benefit
   */
  def getBenefitIterationCount(n: Int = 1): Int = {
    val a = iterate(n)
    if (a > 6) {
      n
    } else {
      getBenefitIterationCount(n + 1)
    }
  }
  
  def iterate(age: Int): Long = {
    if (age <= 0) {
      0
    } else {
      val minAge = math.min(age, 6)
      val children = minAge * 2
      
      val childrenIterates = (1 until minAge).map(a => iterate(a + age - minAge))      
      children + childrenIterates.map(_ * 2).sum
    }
  }
}
