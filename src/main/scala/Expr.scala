/**
  * Created by nickolay on 18.05.17.
  */
trait Expr {
  def isReduciable: Boolean = {
    this match {
      case Bool(n) => false
      case Number(n) => false
      case _ => true
    }


  }

}
