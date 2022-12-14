package com.lab2.java.gui;

import com.lab2.java.doublyLinkedList.DoublyLinkedList;
import com.lab2.java.doublyLinkedList.LinkedList;
import com.lab2.java.doublyLinkedList.ListSerializeDeserialize;

import java.io.FileNotFoundException;

public class ListActionListenerOverride extends AbstractListActionListener {
    protected DoublyLinkedList<Object> items = new LinkedList<>();

    @Override
    public void onAdd(String text) {
        if (text.isEmpty()) return;
        Object data = builder.createFromString(text);
        items.add(data);
        listModel.addElement(data);
    }

    @Override
    public void onInsert(String text, int index) {
        if (text.isEmpty()) return;
        Object data = builder.createFromString(text);
        items.add(data, index);
        listModel.add(index, data);
    }

    @Override
    public void onRemove(int index) {
        items.remove(index);
        listModel.remove(index);
    }

    @Override
    public void onSort() {
        items.sort(builder.getTypeComparator());
        listModel.clear();
        items.forEach(listModel::addElement);
    }

    @Override
    public void onSave() {
        try {
            ListSerializeDeserialize.saveToFile(filename, items, builder);
        } catch (FileNotFoundException e) {
            System.err.println("Нельзя записать список в файл!");
            e.printStackTrace();
        }
    }

    @Override
    public void onLoad() {
        try {
            items = ListSerializeDeserialize.loadFromFile(filename, builder, new LinkedList<>());
            listModel.clear();
            items.forEach(listModel::addElement);
        } catch (Exception e) {
            System.err.println("Нельзя прочитать список из файла!");
            e.printStackTrace();
        }
    }

    @Override
    public void onClearList() {
        items.removeAll();
        listModel.removeAllElements();
    }
}
