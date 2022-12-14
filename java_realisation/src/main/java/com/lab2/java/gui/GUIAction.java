package com.lab2.java.gui;

import javax.swing.*;
import java.awt.*;
import java.util.function.Consumer;

public class GUIAction extends JPanel {

    public GUIAction(EmptyConsumer action, String buttonText) {
        JButton button = new JButton(buttonText);
        button.setForeground(new Color(255,255,255));
        button.setBackground(new Color(50,50,180));
        button.addActionListener(e -> action.accept());
        add(button);
    }

    public GUIAction(Consumer<String> action, String buttonText) {
        JTextField textField = new JTextField();
        textField.setPreferredSize(new Dimension(100, 25));
        JButton button = new JButton(buttonText);
        button.setForeground(new Color(255,255,255));
        button.setBackground(new Color(50,50,180));
        button.addActionListener(e -> {
            action.accept(textField.getText());
            textField.setText("");
        });
        add(textField);
        add(button);
    }
}
