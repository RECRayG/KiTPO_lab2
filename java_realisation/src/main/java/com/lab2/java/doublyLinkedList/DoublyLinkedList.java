package com.lab2.java.doublyLinkedList;

public interface DoublyLinkedList<T> {
    void add(T data);
    T get(int index);
    void remove(int index);
    void removeAll();
    int size();
    void add(T data, int index);
    void forEach(DoWith<T> a);
    void sort(Comparator<T> comparator);
}
