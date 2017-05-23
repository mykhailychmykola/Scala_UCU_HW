import scala.collection.mutable.Map

/**
  * Created by nickolay on 18.05.17.
  */
final class Machine(env: Map[String, Any] = Map()) {
  val environment = this.env

  def reductionStep(e: Expr, env: Map[String, Any]): Expr = {
    e match {
//      case Var(s) => Number(env.getOrElse(s, throw new Error(s"Wrong variable input $s")))
      case Var(s) => {
        val n = env.getOrElse(s, throw new Error(s"Wrong variable input $s"))
        n match {
          case Int => Number(n.asInstanceOf[Int])
          case Boolean => Bool(n.asInstanceOf[Boolean])
          case _ => throw  new Error(s"We don't have such var in environment $s")
        }

      }
      case ifElse(cond, e1, e2) => cond match {
        case Bool(n) => if (e1.isReduciable) {
          ifElse(cond, reductionStep(e1, env), e2)
        }
        else if (e2.isReduciable) {
          ifElse(cond, e1, reductionStep(e2, env))
        }
        else {
          if (n) e1 else e2
        }
        case _ => ifElse(reductionStep(cond, env), e1, e2)
      }

      case Prod(lOp, rOp) => if (lOp.isReduciable) {
        Prod(reductionStep(lOp, env), rOp)
      }
      else if (rOp.isReduciable) {
        Prod(lOp, reductionStep(rOp, env))
      }
      else {
        Prod(lOp, rOp) match {
          case Prod(lOp: Number, rOp: Number) => Number(lOp.n * rOp.n)
          case _ => throw new Error(s"Not supported for this type $lOp, $rOp!")
        }
      }
      case Sum(lOp, rOp) => if (lOp.isReduciable) {
        Sum(reductionStep(lOp, env), rOp)
      } else if (rOp.isReduciable) {
        Sum(lOp, reductionStep(rOp, env))
      }
      else {
        Sum(lOp, rOp) match {
          case Sum(lOp: Number, rOp: Number) => Number(lOp.n + rOp.n)
          case _ => throw new Error(s"Not supported for this type $lOp, $rOp!")
        }
      }
      case LessThen(lOp, rOp) => if (lOp.isReduciable) {
        LessThen(reductionStep(lOp, env), rOp)
      }
      else if (rOp.isReduciable) {
        LessThen(lOp, reductionStep(rOp, env))
      }
      else {
        LessThen(lOp, rOp) match {
          case LessThen(lOp: Number, rOp: Number) => Bool(lOp.n < rOp.n)
          case _ => throw new Error(s"Not supported for this type $lOp, $rOp!")
        }
      }

    }
  }

  def run(expr: Expr, env: Map[String, Any] = this.environment): Expr = {
    println(expr)
    if (expr.isReduciable)
      run(reductionStep(expr, env), env)
    else
      expr
  }


  def execute(s: Statements, env: Map[String, Any] = this.environment): Machine = {
    s match {
      case DoNothing() => this
      case Assign(variable, e) => {
        val a: Expr = run(e, env)
        a match {
          case Number(n) => {
            this.env.put(variable, n)
            this
          }
          case _ => throw new Error(s"Invalid expression for variable $s")
        }
      }
      case Sequences(a@_*) => {
        a.foreach(b => this.execute(b, env))
        this
      }
      case While(e, b) => {
        this.run(e) match {
          case Bool(n) => n match {
            case true => execute(b, env).execute(While(e, b))
            case false => this
          }
          case _ => throw new Error(s"Not correct expression $e should be of type Bool!")

        }
      }
    }
  }
}
