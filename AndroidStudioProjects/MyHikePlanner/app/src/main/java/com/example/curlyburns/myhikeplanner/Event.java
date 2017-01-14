package com.example.curlyburns.myhikeplanner;

/**
 * Created by CurlyBurns on 10/12/2016.
 */
public class Event {
    private String name;
    private String time;
    private String notes;

    public Event(String name, String time, String notes) {
        this.name = name;
        this.time = time;
        this.notes = notes;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getTime() {
        return time;
    }

    public void setTime(String time) {
        this.time = time;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }
}

