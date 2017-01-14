package com.example.curlyburns.mycalender;

import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.app.ListFragment;
import android.content.Context;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.GridView;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import java.util.ArrayList;

/**
 * Created by CurlyBurns on 9/25/2016.
 */
public class FragmentDay extends Fragment {

    private ArrayList<Event> dayEvents;
    private int pos;
    View myView;

    String[] days = {"fluff", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};

    onChangeListener myCallBack;
    public interface onChangeListener{
        public void eventArrayChange(int d, ArrayList<Event> e);
        public void eventChange(int pos, Event e);
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        myView = inflater.inflate(R.layout.day_layout, container, false);

        dayEvents = ((MainActivity)getActivity()).getCurrentDay();
        pos = ((MainActivity)getActivity()).getDay();

        TextView theDay = (TextView) myView.findViewById(R.id.dayLbl);
        theDay.setText(days[pos % 8]);

        populateListView();

        myCallBack = (onChangeListener) getActivity();

        final ImageButton addButton = (ImageButton) myView.findViewById(R.id.addButton);
        addButton.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                dayEvents.add(new Event("NewEvent", "", ""));
                myCallBack.eventArrayChange(pos, dayEvents);
                populateListView();
            }
        });

        return myView;
    }


    private void populateListView() {

        //Create Adapter
        myArrayAdapter<Event> myListAdapter = new myArrayAdapter<Event>();

        //set Adapter
        ListView myList = (ListView) myView.findViewById(R.id.myListView);
        myList.setAdapter(myListAdapter);


    }

    private class myArrayAdapter<T> extends ArrayAdapter<Event> {
        public myArrayAdapter() {
            super(getActivity(), android.R.layout.simple_list_item_1, dayEvents);
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            Button button;
            if (convertView == null) {
                //if not recycled set attributes
                button = new Button(getActivity());
            }
            else
                button = (Button) convertView;

            button.setText(dayEvents.get(position).getName());
            button.setOnClickListener(new View.OnClickListener() {
                public void onClick(View v) {
                    ListView parent = (ListView) v.getParent();
                    final int posn = parent.getPositionForView(v);
                    myCallBack.eventChange(posn, dayEvents.get(posn));
                    FragmentManager fragManager = getFragmentManager();
                    FragmentTransaction fragTrans = fragManager.beginTransaction();
                    FragmentEvent event = new FragmentEvent();
                    fragTrans.replace(R.id.main_activity_layout, event);
                    fragTrans.addToBackStack(null);
                    fragTrans.commit();
                }
            });
            return button;
        }
    }

}
