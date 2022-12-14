package com.lab2.java.doublyLinkedList.build;

import com.lab2.java.doublyLinkedList.Comparator;

import java.util.Random;

public class StringObjectBuilder implements ObjectBuilder {

    private static String getString() {
        int leftLimit = 97; // 'a'
        int rightLimit = 122; // 'z'
        int targetStringLength = 4;
        Random random = new Random();
        StringBuilder buffer = new StringBuilder(targetStringLength);
        for (int i = 0; i < targetStringLength; i++) {
            int randomLimitedInt = leftLimit + (int) (random.nextFloat() * (rightLimit - leftLimit + 1));
            buffer.append((char) randomLimitedInt);
        }
        return buffer.toString();
    }

    @Override
    public String typeName() {
        return "String";
    }

    @Override
    public String create() {
        return getString();
    }

    @Override
    public String createFromString(String s) {
        return s;
    }

    @Override
    public String toString(Object object) {
        return object.toString();
    }

    @Override
    public Comparator<Object> getTypeComparator() {
        return (o1, o2) -> ((String) o1).compareTo((String) o2);
    }
}
