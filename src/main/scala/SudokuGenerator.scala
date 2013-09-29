import java.util
import scala.util.Random
import scala.collection.JavaConversions._

object SudokuGenerator extends App {
  val boxNums = List(
    1,1,1,2,2,2,3,3,3,
    1,1,1,2,2,2,3,3,3,
    1,1,1,2,2,2,3,3,3,
    4,4,4,5,5,5,6,6,6,
    4,4,4,5,5,5,6,6,6,
    4,4,4,5,5,5,6,6,6,
    7,7,7,8,8,8,9,9,9,
    7,7,7,8,8,8,9,9,9,
    7,7,7,8,8,8,9,9,9
  )
  case class Square(row:Int, col:Int, box:Int, var value:Int=0, var possible:List[Int] = shuffle((1 to 9).toList))

  val squares = shuffle((0 to 80).map(n1=>new Square(n1/9, n1%9, boxNums(n1))).toList)
  val history = new util.Stack[Tuple2[Square,List[Square]]]()

  history.push((squares.head, squares.tail))
  while(history.peek()._2.size!=0) {
    // pop selected top possible (if none, pop history)
    val selected = history.peek()._1
    if (selected.possible.size>0) {
      selected.value = selected.possible.head
      selected.possible = selected.possible.tail
      // update possibles
      val cloned = history.peek()._2.map{ sq =>
        if (sq.box==selected.box||sq.row==selected.row||sq.col==selected.col)
          Square(sq.row, sq.col, sq.box, possible=sq.possible.filter(_!=selected.value))
        else
          Square(sq.row, sq.col, sq.box, possible=sq.possible.toList)
      }
      // push next attempt onto stack
      history.push((cloned.head, cloned.tail.sortBy(_.possible.size)))
    } else {
      history.pop()
      selected.value = 0
    }
  }
  printBoard()

  def printBoard() {
    (history.listIterator().map(_._1) ++ history.peek()._2).toList
      .sortBy(sq=>sq.row*9+sq.col)
      .map(sq=>if(sq.value==0) sq.possible.head else sq.value).zipWithIndex.foreach { sq => print(sq._1+"\t"); if (sq._2%9==8) println() }
  }
  def shuffle[A](list:List[A]):List[A] = list.tail.tail.foldLeft(list.take(2)) { (shuf,next) =>
    val split=shuf.splitAt(Random.nextInt(shuf.size)); split._1 ::: next :: split._2
  }
}


