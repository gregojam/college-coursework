package com.example.curlyburns.myhikeplanner;

import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.content.Context;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;


/**
 * Created by CurlyBurns on 10/12/2016.
 */
public class FragmentEvent extends Fragment {

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRetainInstance(true);
    }

    View myView;

    private Event currentEvent;
    private int ev;
    private int day;

    onChangeListener myCallBack;
    public interface onChangeListener{
        public void onSaveChanges(int m, int ev, String title, String time, String notes);
        public void deleteEvent(int m, int ev);
        public void cancelChanges();
    }


    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        myView = inflater.inflate(R.layout.event_layout, container, false);

        currentEvent = ((MapsActivity)getActivity()).getCurrentEvent();
        ev = ((MapsActivity)getActivity()).getEvent();
        day = ((MapsActivity)getActivity()).getMarker();

        EditText event = (EditText) myView.findViewById(R.id.eventEditText);
        EditText time = (EditText) myView.findViewById(R.id.timeEditText);
        EditText notes = (EditText) myView.findViewById(R.id.notesEditText);

        Button saveBtn = (Button) myView.findViewById(R.id.saveBtn);
        Button cancelBtn = (Button) myView.findViewById(R.id.cancelBtn);
        Button deleteBtn = (Button) myView.findViewById(R.id.deleteBtn);

        event.setText(currentEvent.getName());
        time.setText(currentEvent.getTime());
        notes.setText(currentEvent.getNotes());


        myCallBack = (onChangeListener)getActivity();

        saveBtn.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                EditText event = (EditText) myView.findViewById(R.id.eventEditText);
                EditText time = (EditText) myView.findViewById(R.id.timeEditText);
                EditText notes = (EditText) myView.findViewById(R.id.notesEditText);
                myCallBack.onSaveChanges(day, ev,
                        event.getText().toString(),
                        time.getText().toString(),
                        notes.getText().toString());
                FragmentManager fragManager = getFragmentManager();
                fragManager.popBackStack();
            }
        });

        cancelBtn.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                myCallBack.cancelChanges();
                FragmentManager fragManager = getFragmentManager();
                fragManager.popBackStack();
            }
        });

        deleteBtn.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                myCallBack.deleteEvent(day, ev);
                FragmentManager fragManager = getFragmentManager();
                fragManager.popBackStack();
            }
        });

        return myView;
    }
}



