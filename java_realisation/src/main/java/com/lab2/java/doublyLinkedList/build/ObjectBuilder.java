package com.lab2.java.doublyLinkedList.build;

import com.lab2.java.doublyLinkedList.Comparator;

public interface ObjectBuilder extends Build<Object> {
    String typeName();

    Object create();

    Comparator<Object> getTypeComparator();

    Object createFromString(String s);

    String toString(Object object);
}
