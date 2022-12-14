package com.lab2.java.doublyLinkedList;

public class LinkedList<T> implements DoublyLinkedList<T> {

    private Node head;
    private Node tail;
    private int length;

    public void add(T data) {
        if (head == null) {
            head = new Node(data);
            tail = head;
            length++;
            return;
        }
        Node newTail = new Node(data);
        newTail.prev = tail;
        tail.next = newTail;
        tail = newTail;
        length++;
    }

    public T get(int index) {
        return getNode(index).data;
    }

    public void remove(int index) {
        Node tmp = getNode(index);

        if(tmp == head) {
            tmp.prev = null;
            head = tmp.next;
        } else {
            if(tmp.prev != null) {
                tmp.prev.next = tmp.next;
                tmp.prev = null;
            }
        }

        if(tmp == tail) {
            tmp.next = null;
            tail = tmp.prev;
        } else {
            if(tmp.next != null) {
                tmp.next.prev = tmp.prev;
                tmp.next = null;
            }
        }

        tmp.next = null;
        tmp.prev = null;
        tmp.data = null;
        length--;
    }

    public int size() {
        return length;
    }

    public void add(T data, int index) {
        Node tmp = getNode(index);
        Node newNode = new Node(data);
        if (tmp != head) {
            tmp.prev.next = newNode;
            newNode.prev = tmp.prev;
        } else {
            head = newNode;
        }
        newNode.next = tmp;
        tmp.prev = newNode;
        length++;
    }

    public void removeAll() {
        if(head != null) {
            Node tmp = getNode(0);
            while(tmp.next != null) {
                remove(0);
                tmp = getNode(0);
            }
            remove(0);
        }
    }

    public void forEach(DoWith<T> a) {
        Node tmp = head;
        for (int i = 0; i < length; i++) {
            a.doWith(tmp.data);
            tmp = tmp.next;
        }
    }

    public void sort(Comparator<T> comparator) {
        head = mergeSort(head,comparator);
    }

    private Node mergeSort(Node h, Comparator<T> comparator) {
        if (h == null || h.next == null) {
            return h;
        }

        Node middle = getMiddle(h);
        Node middleNext = middle.next;

        middle.next = null;

        Node left = mergeSort(h, comparator);

        Node right = mergeSort(middleNext, comparator);

        return merge(left, right, comparator);
    }

    private Node merge(Node head11, Node head22, Comparator<T> comparator) {
        Node left = head11;
        Node right = head22;
        Node merged = new Node(null);
        Node temp = merged;
        while (left != null && right != null) {
            if (comparator.compare(left.data, right.data) < 0) {
                temp.next = left;
                left.prev = temp;
                left = left.next;
            } else {
                temp.next = right;
                right.prev = temp;
                right = right.next;
            }
            temp = temp.next;
        }
        while (left != null) {
            temp.next = left;
            left.prev = temp;
            left = left.next;
            temp = temp.next;
        }
        while (right != null) {
            temp.next = right;
            right.prev = temp;
            right = right.next;
            temp = temp.next;
            this.tail = temp;
        }
        return merged.next;
    }

    private Node getMiddle(Node h) {
        if (h == null)
            return null;
        Node fast = h.next;
        Node slow = h;

        while (fast != null) {
            fast = fast.next;
            if (fast != null) {
                slow = slow.next;
                fast = fast.next;
            }
        }
        return slow;
    }

    private Node getNode(int index) {
        if (index < 0 || index >= length) throw new IndexOutOfBoundsException();
        Node tmp = head;

        for (int i = 0; i < index; i++) {
            tmp = tmp.next;
        }
        return tmp;
    }

    private class Node {
        T data;
        Node next;
        Node prev;

        public Node(T data) {
            this.data = data;
            next = prev = null;
        }
    }
}
