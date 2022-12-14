package com.lab2.scala.doublyLinkedList

import com.lab2.java.doublyLinkedList.DoWith
import com.lab2.java.doublyLinkedList.Comparator
import com.lab2.java.doublyLinkedList.DoublyLinkedList


class MyLinkedList[T] extends DoublyLinkedList[T] {

  private var head: Node = _
  private var tail: Node = _
  private var length: Int = 0

  def add(data: T): Unit = {
    if (head == null) {
      head = new Node(Some(data))
      tail = head
      length += 1
      return
    }
    val newTail = new Node(Some(data))
    newTail.prev = tail
    tail.next = newTail
    tail = newTail
    length += 1
  }

  def get(index: Int): T = getNode(index).data.get

  def forEach(action: T => Unit): Unit = {
    var tmp = head
    for (_ <- 0 until length) {
      action(tmp.data.get)
      tmp = tmp.next
    }
  }

  def size: Int = length

  def remove(index: Int): Unit = {
    val tmp = getNode(index)
    if (tmp == head) {
      tmp.prev = null
      head = tmp.next
    } else {
      if (tmp.prev != null) {
        tmp.prev.next = tmp.next
        tmp.prev = null
      }
    }

    if (tmp == tail) {
      tmp.next = null
      tail = tmp.prev
    } else {
      if (tmp.next != null) {
        tmp.next.prev = tmp.prev
        tmp.next = null
      }
    }

    tmp.next = null
    tmp.prev = null
    tmp.data = null
    length -= 1
  }

  def removeAll(): Unit = {
    if(head != null) {
      var tmp = getNode(0)
      while({tmp.next != null}) {
        remove(0)
        tmp = getNode(0)
      }
      remove(0)
    }
  }

  def add(data: T, index: Int): Unit = {
    val tmp = getNode(index)
    val newNode = new Node(Some(data))
    if (tmp != null) {
      tmp.prev.next = newNode
      newNode.prev = tmp.prev
    }
    else {
      head = newNode
    }
    newNode.next = tmp
    tmp.prev = newNode
    length += 1
  }

  def forEach(a: DoWith[T]): Unit = {
    var tmp = head
    for (_ <- 0 until length) {
      a.doWith(tmp.data.get)
      tmp = tmp.next
    }
  }

  def sort(comparator: Comparator[T]): Unit = {
    head = mergeSort(head, comparator)
    tail = getNode(length - 1)
  }

  private def mergeSort(h: Node, comparator: Comparator[T]): Node = {
    if (h == null || h.next == null) {
      return h
    }

    val middle = getMiddle(h)
    val middleNext = middle.next

    middle.next = null

    val left = mergeSort(h, comparator)
    val right = mergeSort(middleNext, comparator)

    merge(left, right, comparator)
  }

  private def merge(head11: Node, head22: Node, comparator: Comparator[T]) = {
    var left = head11
    var right = head22
    val merged = new Node(None)
    var temp = merged
    while ( {
      left != null && right != null
    }) {
      if (comparator.compare(left.data.get, right.data.get) < 0) {
        temp.next = left
        left.prev = temp
        left = left.next
      }
      else {
        temp.next = right
        right.prev = temp
        right = right.next
      }
      temp = temp.next
    }
    while ( {
      left != null
    }) {
      temp.next = left
      left.prev = temp
      left = left.next
      temp = temp.next
    }
    while ( {
      right != null
    }) {
      temp.next = right
      right.prev = temp
      right = right.next
      temp = temp.next
      this.tail = temp
    }
    merged.next
  }

  private def getMiddle(h: Node) = {
    var fast = h.next
    var slow = h
    while (fast != null) {
      fast = fast.next
      if (fast != null) {
        slow = slow.next
        fast = fast.next
      }
    }
    slow
  }

  private def getNode(index: Int): Node = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException()
    var tmp = head
    for (_ <- 0 until index) {
      tmp = tmp.next
    }
    tmp
  }

  class Node(var data: Option[T]) {
    var next: Node = _
    var prev: Node = _
  }
}