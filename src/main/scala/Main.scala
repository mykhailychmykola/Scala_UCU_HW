import scala.collection.mutable.Map
/**
  * Created by nickolay on 18.05.17.
  */
object Main {
  def main(args: Array[String]): Unit = {
    val a = new Machine(Map("X" -> 5)).execute(Sequences(Assign("X", Number(5)), Assign("y", Number(9)), Assign("L", Bool(true))))
    print(a.environment)

  }

}
