package com.lab2.java.doublyLinkedList.build;

import com.lab2.java.doublyLinkedList.Comparator;

import java.util.Random;

public class IntegerObjectBuilder implements ObjectBuilder {

    @Override
    public String typeName() {
        return "Integer";
    }

    @Override
    public Integer create() {
        return new Random().nextInt(100);
    }

    @Override
    public Integer createFromString(String s) {
        return Integer.parseInt(s);
    }

    @Override
    public String toString(Object object) {
        return object.toString();
    }

    @Override
    public Comparator<Object> getTypeComparator() {
        return (o1, o2) -> (Integer) o1 - (Integer) o2;
    }

}
