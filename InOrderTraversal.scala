/**
 * Fun learning class for Scala!
 * In order traversal (on a tree that does not have values on the internal nodes).
 */
abstract class Node[T]
case class Parent[T](leftChild: Node[T], rightChild: Node[T]) extends Node[T]
case class Leaf[T](value: T) extends Node[T]
object InOrderTraversal extends App {
    def traverseTree[T,U](node: Node[T], map: T => U, reduce: (U,U) => U): U = {
        node match {
            case Parent(leftChild, rightChild) =>
                reduce(traverseTree(leftChild, map, reduce), traverseTree(rightChild, map, reduce))
            case Leaf(value) =>
                map(value)
        }
    }
    val tree = Parent(Leaf(1), Parent(Parent(Leaf(3), Leaf(4)), Leaf(2)))
    println(traverseTree(tree, {x: Int => x.toString}, {(x: String, y: String) => x + ", " + y}))
}
