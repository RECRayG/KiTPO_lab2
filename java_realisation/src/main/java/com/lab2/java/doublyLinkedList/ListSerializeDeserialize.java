package com.lab2.java.doublyLinkedList;

import com.lab2.java.doublyLinkedList.build.Build;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;

public class ListSerializeDeserialize {

    public static <T> void saveToFile(String filename, DoublyLinkedList<T> list, Build<T> builder) throws FileNotFoundException {
        String fullPath = "";

        if(!Files.exists(Paths.get("stringData"))) {
            File filePath = new File("stringData");
            filePath.mkdir();
        }
        if(!Files.exists(Paths.get("integerData"))) {
            File filePath = new File("integerData");
            filePath.mkdir();
        }

        if(builder.typeName().equals("String"))
            fullPath = "stringData\\" + filename;
        else
            fullPath = "integerData\\" + filename;

        try (PrintWriter writer = new PrintWriter(fullPath)) {
            writer.println(builder.typeName());
            list.forEach(el -> writer.println(builder.toString(el)));
        }
    }

    public static <T> DoublyLinkedList<T> loadFromFile(String filename, Build<T> builder, DoublyLinkedList<T> list) throws Exception {
        String fullPath = "";
        if(builder.typeName().equals("String") && Files.exists(Paths.get("stringData\\" + filename)))
            fullPath = "stringData\\" + filename;
        else {
            if(!builder.typeName().equals("Integer"))
                throw new Exception("Загружаемый файл отсутствует!");
        }

        if(builder.typeName().equals("Integer") && Files.exists(Paths.get("integerData\\" + filename)))
            fullPath = "integerData\\" + filename;
        else {
            if(!builder.typeName().equals("String"))
                throw new Exception("Загружаемый файл отсутствует!");
        }

        try (BufferedReader br = new BufferedReader(new FileReader(fullPath))) {
            String line;

            line = br.readLine();
            if (!builder.typeName().equals(line)) {
                throw new Exception("Неверная структура файла!");
            }

            while ((line = br.readLine()) != null) {
                list.add(builder.createFromString(line));
            }
            return list;
        }
    }
}
