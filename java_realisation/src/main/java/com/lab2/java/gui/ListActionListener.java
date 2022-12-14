package com.lab2.java.gui;

import javax.swing.*;

public interface ListActionListener {

    void onAdd(String text);

    void onInsert(String text, int index);

    void onRemove(int index);

    void onSort();

    void onSave();

    void onLoad();

    DefaultListModel<Object> getListModel();

    void onSelectType(String type);

    void onClearList();
}
