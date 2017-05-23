/**
  * Created by nickolay on 18.05.17.
  */
import org.scalatest._
import scala.collection.mutable.Map

class MachineTest extends FunSpec {
  describe("Reduction model") {
    it("should reduce `(1 + 2) + (4 + 3)` to 3 + (4 + 3)") {
      assert(
        new Machine().reductionStep(
          Prod(
            Sum(
              Number(1),
              Number(2)),
            Sum(
              Number(4),
              Number(3))),
          Map())
          ==
          Prod(
            Number(3),
            Sum(
              Number(4),
              Number(3))))
    }

    it("should reduce `3 + (5 + (-3))` to 3 + 2") {
      assert(
        new Machine().reductionStep(
          Sum(
            Number(3),
            Sum(
              Number(5),
              Number(-3))), Map())
          ===
          Sum(
            Number(3),
            Number(2)))
    }

    it("should reduce `((1 + 1) * 1 + 1) * 1` to `(2 * 1 + 1) * 1`") {
      assert(
        new Machine().reductionStep(
          Prod(
            Sum(
              Prod(
                Sum(
                  Number(1),
                  Number(1)),
                Number(1)),
              Number(1)),
            Number(1)),
          Map())
          ===
          Prod(
            Sum(
              Prod(
                Number(2),
                Number(1)),
              Number(1)),
            Number(1)))
    }

    it("should reduce `(x + 2) * (4 + y)` where x = 1 and y = 3 to `(1 + 2) * (4 + y)`") {
      assert(
        new Machine().reductionStep(
          Prod(
            Sum(
              Var("x"),
              Number(2)),
            Sum(
              Number(4),
              Var("y"))),
          Map("x" -> 1,
            "y" -> 3))
          ===
          Prod(
            Sum(
              Number(1),
              Number(2)),
            Sum(
              Number(4),
              Var("y"))))
    }
  }

  describe("Expression evaluation") {
    describe("Primitive values") {
      it("should not reduce Bool expression") {
        assert(
          new Machine().run(
            Bool(true),
            Map()) == Bool(true))
      }

      it("should not reduce Number expression") {
        assert(
          new Machine().run(
            Number(42),
            Map()) == Number(42))
      }
    }

    describe("Sum") {
      it("should reduce 3 + 5 expression") {
        assert(
          new Machine().run(
            Sum(
              Number(3),
              Number(5)),
            Map()) === Number(8))
      }

      it("should reduce 3 + 5 - 3 expression") {
        assert(
          new Machine().run(
            Sum(
              Number(3),
              Sum(
                Number(5),
                Number(-3))),
            Map()) === Number(5))
      }
    }

    describe("Prod") {
      it("should reduce 3 * 5 expression") {
        assert(
          new Machine().run(
            Prod(
              Number(3),
              Number(5)),
            Map()) === Number(15))
      }
    }


    describe("LessThan") {
      it("should reduce 3 < 5 expression to Bool true") {
        assert(
          new Machine().run(
            LessThen(
              Number(3),
              Number(5)),
            Map()) === Bool(true))
      }

      it("should reduce 5 < 5 expression to Bool false") {
        assert(
          new Machine().run(
            LessThen(
              Number(5),
              Number(5)),
            Map()) === Bool(false))
      }
    }


    describe("If-Else") {
      it("should reduce If-Else expression to Number 42") {
        assert(
          new Machine().run(
            ifElse(
              Bool(true),
              Number(42),
              Number(43)),
            Map()) === Number(42))
      }

      it("should reduce If-Else expression to Number 43") {
        assert(
          new Machine().run(
            ifElse(
              Bool(false),
              Number(42),
              Sum(
                Number(42),
                Number(1))),
            Map()) === Number(43))
      }

      it("should reduce If-Else expression to Number 42 if condition LessThan expression") {
        assert(
          new Machine().run(
            ifElse(
              LessThen(
                Number(3),
                Number(5)),
              Number(42),
              Number(43)),
            Map()) === Number(42))
      }
    }

    describe("Combination of expressions") {
      it("should reduce (1 + 2) < (3 * 4) to `true`") {
        assert(
          new Machine().run(
            LessThen(
              Sum(
                Number(1),
                Number(2)),
              Prod(
                Number(3),
                Number(4))),
            Map()) === Bool(true))
      }

      it("should reduce `(1 + 2) * (4 + 3)` to `21`") {
        assert(
          new Machine().run(
            Prod(
              Sum(
                Number(1),
                Number(2)),
              Sum(
                Number(4),
                Number(3))),
            Map())
            ===
            Number(21))
      }

      it("should calculate ((1 + 1) * 1 + 1) * 1") {
        assert(
          new Machine().run(
            Prod(
              Sum(
                Prod(
                  Sum(
                    Number(1),
                    Number(1)),
                  Number(1)),
                Number(1)),
              Number(1)),
            Map()) === Number(3))
      }

      it("should calculate (x + 2) * (4 + y) where x = 1 and y = 3") {
        assert(
          new Machine().run(
            Prod(
              Sum(
                Var("x"),
                Number(2)),
              Sum(
                Number(4),
                Var("y"))),
            Map("x" -> 1,
              "y" -> 3)) === Number(21))
      }

      it("should calculate if (42 < 43) { 42 } else { 43 < 42 } to false") {
        assert(
          new Machine().run(
            ifElse(
              LessThen(
                Number(43),
                Number(42)),
              Number(42),
              Sum(
                Number(42),
                Number(42))),
            Map()) === Number(84))
      }

      it("should calculate (1 + 2 + 3) * (4 + 5)") {
        assert(
          new Machine().run(
            Prod(
              Sum(
                Sum(
                  Number(1),
                  Number(2)),
                Number(3)),
              Sum(
                Number(4),
                Number(5))),
            Map()) === Number(54))
      }
    }
  }

  describe("Statements execution") {
    it("two empty machines is equal") {
      assert(
        new Machine()
          .execute(DoNothing()).environment
          ===
          new Machine(Map())
            .execute(DoNothing())
            .environment)
    }
  }

  it("should add value to variable") {
    assert(
      new Machine(Map("x" -> 0)).execute(Assign("x", Number(1))).environment
        ===
        new Machine(Map("x" -> 1)).execute(DoNothing()).environment)
  }


  it("should execute sequance of statements") {
    assert(
      new Machine(Map("x" -> 0)).execute(
        Sequences(
          Assign("x", Number(1)),
          DoNothing(),
          Assign("x", Number(2)))).environment
        ===
        new Machine(Map("x" -> 2)).execute(DoNothing()).environment)
  }


    it("should execute while statement") {
      assert(
        new Machine(Map("x" -> 0, "y" -> 0)).execute(
          While(
            LessThen(Var("x"), Number(10)),
            Sequences(
              Assign("x", Sum(Var("x"), Number(1))),
              Assign("y", Sum(Var("y"), Number(2)))
            )
          )).environment

          ===
          new Machine(Map("x" -> 10, "y" -> 20)).execute(DoNothing()).environment)
    }

  describe("General test for expressions"){
    it("Number does not reduce") {
      assert(new Machine().run(Number(4)) === Number(4));
    }
    it("Bool does not reduce") {
    assert(new Machine().run(Bool(true)) === Bool(true));
    }
  }

}



//
///////////////////////////// EXPRESSIONS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
//test("Number does not reduce") {
//  test("Bool does not reduce") {
//    test("Number Var reduces to its value") {
//      test("Bool Var reduces to its value") {
//        test("Unknown Var does not reduce") {
//
//          // SUM
//          test("Sum of two Numbers reduces to Number with their sum") {
//            test("Sum of Number and Bool does not reduce") {
//              test("Sum of Bool and Number does not reduce") {
//                test("left Sum operand reduces if it is reducible and right is left unchanged") {
//                  test("otherwise right Sum operand reduces") {
//
//                    // PROD
//                    test("Prod of two Numbers reduces to Number with their product") {
//                      test("Prod of Number and Bool does not reduce") {
//                        test("Prod of Bool and Number does not reduce") {
//                          test("left Prod operand reduces if it is reducible and right is left unchanged") {
//                            test("otherwise right Prod operand reduces") {
//
//                              // LESS
//                              test("Less of two Numbers reduces to Bool indicating whether first number is less than the second") {
//                                test("Less of Number and Bool does not reduce") {
//                                  test("Less of Bool and Number does not reduce") {
//                                    test("left Less operand reduces if it is reducible and right is left unchanged") {
//                                      test("otherwise right Less operand reduces") {
//
//                                        // IfElse
//                                        test("IfElse reduces to thenExpr for Bool(true) condition") {
//                                          test("IfElse reduces to elseExpr for Bool(false) condition") {
//                                            test("IfElse for Number condition does not reduce") {
//                                              test("IfElse for reducible condition reduces its condition") {
//
//                                                /////////////////////////// STATEMENTS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
//                                                test("DoNothing does not alter environment") {
//
//                                                  // Assign
//                                                  test("Assign adds new variable for number expression") {
//                                                    test("Assign adds new variable for boolean expression") {
//                                                      test("Assign updates existing variable for number expression") {
//                                                        test("Assign updates existing variable for boolean expression") {
//                                                          test("Assign updates existing variable for expression with the same variable") {
//                                                            test("Assign does not occur for erroneous expression") {
//
//                                                              // If
//                                                              test("'If' runs thenStat if condition is Bool(true)") {
//                                                                test("'If' runs elseStat if condition is Bool(false)") {
//                                                                  test("'If' statement fails for erroneous condition") {
//                                                                    test("'If' statement fails for condition expression that reduces to Number") {
//
//                                                                      // Seq
//                                                                      test("'Seq' does nothing if empty") {
//                                                                        test("'Seq' executes one its statement if contains only one") {
//                                                                          test("'Seq' executes its statements one by one") {
//                                                                            test("'Seq' does not execute remained statements after first failure") {
//
//                                                                              // While
//                                                                              test("'While' executes thenStat multiple times while condition reduces to Bool(true)") {
//                                                                                test("'While' does not execute thenStat if condition reduces to Bool(false) from the start") {
//                                                                                  test("'While' statement fails for erroneous condition") {
//                                                                                    test("'While' statement fails for condition expression that reduces to Number") {
//                                                                                      test("'While' statement fails if thenStat statement fails") {