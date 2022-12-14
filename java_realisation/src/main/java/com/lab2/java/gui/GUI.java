package com.lab2.java.gui;

import com.lab2.java.doublyLinkedList.ObjectBuilderFactory;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;

public class GUI extends JFrame {
    private final JList<Object> jList;
    protected Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
    private JPanel listPanel = new JPanel();
    private JPanel rightPanel = new JPanel();
    private JPanel addPanel = new JPanel();
    private JPanel chooseType = new JPanel();
    private JScrollPane addPanelScroll;
    private JButton backButton = new JButton("< Назад");
    private JComboBox<String> comboBox;

    public GUI(ListActionListener actionListener) throws HeadlessException {
        this.jList = new JList<>(actionListener.getListModel());
        backButton.setEnabled(false);
        backButton.setForeground(new Color(255,255,255));
        backButton.setBackground(new Color(50,50,180));
        addPanel.setLayout(new MigLayout());
        addPanel.setBackground(new Color(155,155,155));
        rightPanel.setBackground(new Color(155,155,155));
        listPanel.setBackground(new Color(155,155,155));

//        Container container = getContentPane();
//        container.setLayout(new BorderLayout());

        listPanel.add(new JScrollPane(jList));
        rightPanel.setLayout(new MigLayout());

        rightPanel.add(chooseType);
        comboBox = new JComboBox<>(ObjectBuilderFactory.getTypeNameList().toArray(new String[0]));
        comboBox.setForeground(new Color(50,50,180));
        comboBox.setBackground(new Color(255,255,255));
        chooseType.setLayout(new MigLayout());
        chooseType.add(comboBox, "w 242!");
        comboBox.addActionListener(actionEvent -> {
            backButton.setEnabled(true);
            JComboBox source = (JComboBox) actionEvent.getSource();
            String selectedItem = (String) source.getSelectedItem();
            actionListener.onSelectType(selectedItem);
            rightPanel.add(new GUIAction(actionListener::onAdd, "Добавить элемент"), "wrap, al right");
            rightPanel.add(new GUIAction(() -> actionListener.onRemove(jList.getSelectedIndex()), "Удалить элемент"), "wrap, al right");
            rightPanel.add(new GUIAction(actionListener::onClearList, "Удалить все элементы"), "wrap, al right");
            rightPanel.add(new GUIAction((text) -> actionListener.onInsert(text, jList.getSelectedIndex()), "Вставить элемент"), "wrap, al right");
            rightPanel.add(new GUIAction(actionListener::onSort, "Отсортировать"), "wrap, al right");
            rightPanel.add(new GUIAction(actionListener::onSave, "Сохранить"), "split 2, al center");
            rightPanel.add(new GUIAction(actionListener::onLoad, "Загрузить"), "wrap");
            rightPanel.remove(chooseType);
            revalidate();
            repaint();
        });
        backButton.addActionListener(actionEvent -> {
            backButton.setEnabled(false);
            actionListener.onClearList();
            rightPanel.removeAll();
            rightPanel.add(chooseType);
            revalidate();
            repaint();
        });


        addPanel.add(backButton, "gaptop 50, gapleft 20, wrap");
        addPanel.add(listPanel, "gapleft 200, wrap");
        addPanel.add(rightPanel, "gapleft 200");

        addPanelScroll = new JScrollPane(addPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        addRegistryAddWindow(addPanelScroll, (int)(screenSize.getWidth()), (int)(screenSize.getHeight()));
    }

    private void addRegistryAddWindow(JScrollPane _panel, int width, int height)
    {
        this.setBounds(0, 0, width / 2, (int) (height / 1.2)); // Устанавливаем оптимальные размеры окна, учитывая размеры экрана
        //this.setExtendedState(this.MAXIMIZED_BOTH); // Растяжение на весь экран
        this.setTitle("Двусвязный разомкнутый список"); // Заголовок окна
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // При нажатии на X ничего не произойдёт
        this.setFocusable(true); // Фокус на окне
        this.setResizable(false); // Запрет на изменение размеров окна - нет
        //this.setModal(true); // Модальное окно
        this.setLocationRelativeTo(null); // Расположение окна ровно по центру
        //this.setTitle("ОШИБКА!!!"); // Заголовок окна
        this.setLayout(new MigLayout());
        this.add(_panel, "grow, push");
        this.setVisible(true); // Делаем окно видимым
    }
}
