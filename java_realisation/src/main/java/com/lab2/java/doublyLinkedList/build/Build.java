package com.lab2.java.doublyLinkedList.build;

import com.lab2.java.doublyLinkedList.Comparator;

public interface Build<T> {
    String typeName();

    T create();

    Comparator<T> getTypeComparator();

    T createFromString(String s);

    String toString(T object);
}
