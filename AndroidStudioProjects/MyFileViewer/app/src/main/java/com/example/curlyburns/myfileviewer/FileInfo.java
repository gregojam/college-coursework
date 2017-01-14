package com.example.curlyburns.myfileviewer;

import android.content.Context;
import android.widget.TextView;
import android.widget.Toast;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * Created by CurlyBurns on 9/18/2016.
 */
public class FileInfo {
    private String name;
    private String date;
    private String size;

    public FileInfo(String name, String date, String size) {
        this.name = name;
        this.date = date;
        this.size = size;


    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public String getSize() {
        return size;
    }

    public void setSize(String size) {
        this.size = size;
    }

}
