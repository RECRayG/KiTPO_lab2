package com.lab2.scala

import com.lab2.java.doublyLinkedList.ObjectBuilderFactory
import com.lab2.java.gui.GUI
import com.lab2.scala.doublyLinkedList.MyLinkedList
import com.lab2.scala.gui.ListActionListenerOverride


object Main{
  def main(args: Array[String]): Unit = {
    val builder = ObjectBuilderFactory.getBuilderByName("Integer")
    val lst = new MyLinkedList[AnyRef]()

    println("\nИнициализация двусвязного разомкнутого списка")
    lst.add(builder.create())
    lst.add(builder.create())
    lst.add(builder.create())
    lst.add(builder.create())
    lst.add(builder.create())
    lst.forEach(println)

    lst.sort(builder.getTypeComparator)
    println("\nСортировка")
    lst.forEach(println)

    val overr = new ListActionListenerOverride()
    new GUI(overr)
  }
}