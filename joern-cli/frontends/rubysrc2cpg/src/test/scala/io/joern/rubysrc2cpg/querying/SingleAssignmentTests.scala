package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Literal}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class SingleAssignmentTests extends RubyCode2CpgFixture {

  "`=` is represented by an `assignment` operator call" in {
    val cpg = code("""
                     |x = 1
                     |""".stripMargin)

    val List(assignment) = cpg.assignment.l
    assignment.code shouldBe "x = 1"
    assignment.lineNumber shouldBe Some(2)
    assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(lhs, rhs) = assignment.argument.l
    lhs.code shouldBe "x"
    lhs.order shouldBe 1
    lhs.argumentIndex shouldBe 1

    rhs.code shouldBe "1"
    rhs.order shouldBe 2
    rhs.argumentIndex shouldBe 2
  }

  "`+=` is represented by an `assignmentPlus` operator call" in {
    val cpg = code("""
                     |x += 1
                     |""".stripMargin)

    val List(assignment) = cpg.call(Operators.assignmentPlus).l
    assignment.code shouldBe "x += 1"
    assignment.lineNumber shouldBe Some(2)
    assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(lhs, rhs) = assignment.argument.l
    lhs.code shouldBe "x"
    rhs.code shouldBe "1"
  }

  "`||=` is represented by an `assignmentOr` operator call" in {
    val cpg = code("""
        |x ||= false
        |""".stripMargin)

    val List(assignment) = cpg.call(Operators.assignmentOr).l
    assignment.code shouldBe "x ||= false"
    assignment.lineNumber shouldBe Some(2)
    assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(lhs, rhs) = assignment.argument.l
    lhs.code shouldBe "x"
    rhs.code shouldBe "false"
  }

  "`&&=` is represented by an `assignmentAnd` operator call" in {
    val cpg = code("""
        |x &&= true
        |""".stripMargin)

    val List(assignment) = cpg.call(Operators.assignmentAnd).l
    assignment.code shouldBe "x &&= true"
    assignment.lineNumber shouldBe Some(2)
    assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(lhs, rhs) = assignment.argument.l
    lhs.code shouldBe "x"
    rhs.code shouldBe "true"
  }

  "`/=` is represented by an `assignmentDivision` operator call" in {
    val cpg = code("""
        |x /= 10
        |""".stripMargin)

    val List(assignment) = cpg.call(Operators.assignmentDivision).l
    assignment.code shouldBe "x /= 10"
    assignment.lineNumber shouldBe Some(2)
    assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(lhs, rhs) = assignment.argument.l
    lhs.code shouldBe "x"
    rhs.code shouldBe "10"
  }

  "`=` is right-associative" in {
    val cpg = code("""
                     |x = y = 1
                     |""".stripMargin)

    val List(xAssignment, yAssignment) = cpg.assignment.l
    xAssignment.code shouldBe "x = y = 1"
    yAssignment.code shouldBe "y = 1"

    val List(x, yAssignment_) = xAssignment.argument.l
    yAssignment_ shouldBe yAssignment

    val List(y, one) = yAssignment.argument.l
    x.code shouldBe "x"
    y.code shouldBe "y"
    one.code shouldBe "1"
  }

  "`=` binds tighter than `or`" in {
    val cpg = code("""
                     |x = 1 or 2
                     |""".stripMargin)

    val List(or)                = cpg.call(Operators.logicalOr).l
    val List(xAssignment)       = cpg.call(Operators.assignment).l
    val List(xAssignment_, two) = or.argument.l

    xAssignment shouldBe xAssignment_
    xAssignment_.code shouldBe "x = 1"
    or.code shouldBe "x = 1 or 2"
    two.code shouldBe "2"
  }

}
