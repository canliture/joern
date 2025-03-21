package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.Ignore

class MemberTests extends JimpleCode2CpgFixture {

  val cpg: Cpg = code("""
      |class Foo {
      |  int x;
      |}
      |""".stripMargin).cpg

  "should contain MEMBER node with correct properties" in {
    val List(x) = cpg.member("x").l
    x.name shouldBe "x"
    x.code shouldBe "int x"
    x.typeFullName shouldBe "int"
  }

  "should allow traversing from MEMBER to TYPE_DECL" in {
    val List(x) = cpg.member.typeDecl.l
    x.name shouldBe "Foo"
  }

  val cpg2 = code("""
      |class ModifiersTest {
      |    private static final String finalPrivateString = "PRIVATE_STATIC_FINAL";
      |    public String publicString = "PS";
      |    protected int mInt;
      |    long mLong;
      |    public final int FLAG = 1;
      |}
      |""".stripMargin).cpg
  "should have modifiers for MEMBER" in {
    val members                 = cpg2.typeDecl.name("ModifiersTest").member.l
    val finalPrivateStringField = members.find(_.name == "finalPrivateString").get
    finalPrivateStringField.modifier.map(_.code).l shouldBe List("private", "static", "final")
    val publicStringField = members.find(_.name == "publicString").get
    publicStringField.modifier.map(_.code).l shouldBe List("public")
    val protectedField = members.find(_.name == "mInt").get
    protectedField.modifier.modifierType.l shouldBe List("PROTECTED")
    val privateField = members.find(_.name == "mLong").get
    privateField.modifier.size shouldBe 0

  }
  "should have constantValue in code for final or final static MEMBER" in {
    val members                 = cpg2.typeDecl.name("ModifiersTest").member.l
    val finalPrivateStringField = members.find(_.name == "finalPrivateString").get
    val publicStringField       = members.find(_.name == "publicString").get
    val methodInit              = cpg2.method.name("<init>").head
    methodInit.ast.code.l.contains("this.publicString = \"PS\"") shouldBe true
    publicStringField.code shouldBe "java.lang.String publicString"
    finalPrivateStringField.code shouldBe "java.lang.String finalPrivateString = \"PRIVATE_STATIC_FINAL\""
    val flag = members.find(_.name == "FLAG").get
    flag.code shouldBe "int FLAG = 1"
  }

}
