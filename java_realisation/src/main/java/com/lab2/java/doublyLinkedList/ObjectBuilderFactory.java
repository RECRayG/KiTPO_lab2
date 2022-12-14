package com.lab2.java.doublyLinkedList;

import org.reflections.Reflections;
import com.lab2.java.doublyLinkedList.build.ObjectBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class ObjectBuilderFactory {
    private final static List<ObjectBuilder> builders = new ArrayList<>();

    static {
        Reflections reflections = new Reflections("com.lab2.java.doublyLinkedList.build");
        Set<Class<? extends ObjectBuilder>> buildersClasses = reflections.getSubTypesOf(ObjectBuilder.class);
        buildersClasses.forEach(bc -> {
            try {
                ObjectBuilder objectBuilder = bc.getDeclaredConstructor().newInstance();
                builders.add(objectBuilder);
            } catch (Exception ignored) {
                throw new RuntimeException("Что-то полшло не так...");
            }
        });
        System.out.printf("%d типа данных было добавлено\n", builders.size());
    }

    public static Set<String> getTypeNameList() {
        return builders.stream().map(ObjectBuilder::typeName).collect(Collectors.toSet());
    }

    public static ObjectBuilder getBuilderByName(String name) {
        if (name == null) throw new NullPointerException();
        for (ObjectBuilder b : builders) {
            if (name.equals(b.typeName())) return b;
        }
        throw new IllegalArgumentException();
    }
}
