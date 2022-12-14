package com.lab2.scala.gui

import com.lab2.java.doublyLinkedList.DoublyLinkedList
import com.lab2.java.doublyLinkedList.LinkedList
import com.lab2.java.doublyLinkedList.ListSerializeDeserialize
import com.lab2.java.gui.AbstractListActionListener
import com.lab2.scala.doublyLinkedList.MyLinkedList

import java.io.FileNotFoundException

class ListActionListenerOverride extends AbstractListActionListener {

  var items: DoublyLinkedList[Object] = new LinkedList[Object]

  override def onAdd(text: String): Unit = {
    if (text.isEmpty) return
    val value = builder.createFromString(text)
    items.add(value)
    listModel.addElement(value)
  }

  override def onInsert(text: String, index: Int): Unit = {
    if (text.isEmpty) return
    val value = builder.createFromString(text)
    items.add(value, index)
    listModel.add(index, value)
  }

  override def onRemove(index: Int): Unit = {
    items.remove(index)
    listModel.remove(index)
  }

  override def onSort(): Unit = {
    items.sort(builder.getTypeComparator)
    listModel.clear()
    items.forEach(el => listModel.addElement(el))
  }

  override def onSave(): Unit = {

    try ListSerializeDeserialize.saveToFile(filename, items, builder)
    catch {
      case e: FileNotFoundException =>
        System.err.println("Невозможно записать список в файл!")
        e.printStackTrace()
    }
  }

  override def onLoad(): Unit = {
    try {
      items = ListSerializeDeserialize.loadFromFile(filename, builder, new LinkedList[AnyRef])
      listModel.clear()
      items.forEach(el => listModel.addElement(el))
    } catch {
      case e: Exception =>
        System.err.println("Невозможно прочитоть список из файла!")
        e.printStackTrace()
    }
  }

  override def onClearList(): Unit = {
    items.removeAll()
    listModel.removeAllElements()
  }
}
