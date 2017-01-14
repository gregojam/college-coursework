package com.example.curlyburns.mycalender;

import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.graphics.Color;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.GridView;
import android.widget.TextView;

import java.util.ArrayList;

public class MainActivity extends AppCompatActivity implements FragmentDay.onChangeListener,
                                                                FragmentEvent.onChangeListener {

    private String[] gridText = {
                "Day", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat",
                "Events"};

    private ArrayList<ArrayList> eventList = new ArrayList<>();
    private ArrayList<Event> currentDay;


    private Event currentEvent;

    private int day;

    private int event;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        for(int i = 0; i < 16 ; i++) {
            if (i < 9) {
                Event temp = new Event(gridText[i], "", "");
                ArrayList<Event> tempList = new ArrayList<>();
                tempList.add(temp);
                eventList.add(tempList);
            } else
                eventList.add(new ArrayList<Event>());
        }


        //Add Fragment Manager
        FragmentManager fragManager = getFragmentManager();

        //Setup Fragment Transaction
        FragmentTransaction fragTrans = fragManager.beginTransaction();

        populateGridView();
        registerGridViewClicks();

    }

    private void populateGridView() {

        //Create Adapter
        ArrayAdapter<ArrayList> myGridAdapter = new myArrayAdapter<ArrayList>();

        //set Adapter
        GridView myGrid = (GridView)findViewById(R.id.gridView);
        myGrid.setAdapter(myGridAdapter);

    }

    public ArrayList<Event> getCurrentDay() {
        return currentDay;
    }

    public int getDay() {
        return day;
    }

    public Event getCurrentEvent() {
        return currentEvent;
    }

    public int getEvent() {
        return event;
    }

    @Override
    public void eventArrayChange(int d, ArrayList<Event> e) {
        populateGridView();
        eventList.set(d, e);
    }

    @Override
    public void eventChange(int pos, Event e){
        event = pos;
        currentEvent = e;
    }

    @Override
    public void onSaveChanges(int day, int ev, String title, String time, String notes){
        ((Event) eventList.get(day).get(ev)).setName(title);
        ((Event) eventList.get(day).get(ev)).setTime(time);
        ((Event) eventList.get(day).get(ev)).setNotes(notes);
    }

    @Override
    public void deleteEvent(int day, int ev){
        eventList.get(day).remove(ev);
        populateGridView();
    }

    private class myArrayAdapter<T> extends ArrayAdapter<ArrayList> {
        public myArrayAdapter() {
            super(MainActivity.this, android.R.layout.simple_list_item_1, eventList);
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent){
            TextView text;
            if(convertView == null) {
                //if not recycled set attributes
                text = new TextView(MainActivity.this);
                text.setLayoutParams(new GridView.LayoutParams(GridView.AUTO_FIT, 100));
            }
            else
                text = (TextView) convertView;

            if(position == 0 || position == 8) {
                text.setText(((Event)eventList.get(position).get(0)).getName());
                text.setBackgroundColor(Color.CYAN);
            }
            else if(position < 8) {
                text.setText(((Event)eventList.get(position).get(0)).getName());
                text.setBackgroundColor(Color.YELLOW);
            }
            else
                text.setText(((Integer) ((ArrayList)eventList.get(position)).size()).toString());

            return text;

        }
    }

    private void registerGridViewClicks(){
        GridView myGrid = (GridView)findViewById(R.id.gridView);
        myGrid.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                TextView text = (TextView) view;
                if(position > 8) {
                    currentDay = eventList.get(position);
                    day = position;
                    FragmentManager fragManager = getFragmentManager();
                    FragmentTransaction fragTrans = fragManager.beginTransaction();
                    int count = fragManager.getBackStackEntryCount();
                    for(int i = 0; i < count; ++i) {
                        fragManager.popBackStack();
                    }
                    FragmentDay aDay = new FragmentDay();
                    fragTrans.replace(R.id.main_activity_layout, aDay);
                    fragTrans.addToBackStack(null);
                    fragTrans.commit();
                }
            }
        });
    }

}