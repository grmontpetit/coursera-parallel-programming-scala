package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

//  test("balanceV2 should work for empty string") {
//    def check(input: String, expected: Boolean) =
//      assert(balanceV2(input.toArray) == expected,
//        s"balanceV2($input) should be $expected")
//
//    check("", true)
//  }
//
//  test("balanceV2 should work for string of length 1") {
//    def check(input: String, expected: Boolean) =
//      assert(balanceV2(input.toArray) == expected,
//        s"balanceV2($input) should be $expected")
//
//    check("(", false)
//    check(")", false)
//    check(".", true)
//  }
//
//  test("balanceV2 should work for string of length 2") {
//    def check(input: String, expected: Boolean) =
//      assert(balanceV2(input.toArray) == expected,
//        s"balanceV2($input) should be $expected")
//
//    check("()", true)
//    check(")(", false)
//    check("((", false)
//    check("))", false)
//    check(".)", false)
//    check(".(", false)
//    check("(.", false)
//    check(").", false)
//  }
//
//
//  test("more complex balanced with balance") {
//    val bal1 = """(if (zero? x) max (/ 1 x))"""
//    val bal2 = """|I told him (that it's not (yet) done). (But he wasn't listening)"""
//    assert(balance(bal1.toCharArray), true)
//    assert(balance(bal2.toCharArray), true)
//  }
//
//  test("more complex balanced with balanceV2") {
//    val bal1 = """(if (zero? x) max (/ 1 x))"""
//    val bal2 = """|I told him (that it's not (yet) done). (But he wasn't listening)"""
//    assert(balanceV2(bal1.toCharArray), true)
//    assert(balanceV2(bal2.toCharArray), true)
//  }
//
//  test("more complex unbalanced with balance") {
//    val unBal1 = """(o_()"""
//    val unBal2 = """|:-)"""
//    val unBal3 = """|())("""
//    val x = balance(unBal1.toCharArray)
//    assert(balance(unBal1.toCharArray) === false)
//    assert(balance(unBal2.toCharArray) === false)
//    assert(balance(unBal3.toCharArray) === false)
//  }
//
//  test("more complex unbalanced with balanceV2") {
//    val unBal1 = """(o_()"""
//    val unBal2 = """|:-)"""
//    val unBal3 = """|())("""
//    assert(balanceV2(unBal1.toCharArray) === false)
//    assert(balanceV2(unBal2.toCharArray) === false)
//    assert(balanceV2(unBal3.toCharArray) === false)
//  }
}