package com.example.curlyburns.myhikeplanner;


import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.app.ListFragment;
import android.content.Context;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.GridView;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.google.android.gms.maps.model.Marker;

import java.util.ArrayList;

/**
 * Created by CurlyBurns on 10/12/2016.
 */
public class FragmentLedger extends Fragment {

    private ArrayList<Event> markerEvents;
    private int pos;
    private Marker thisMarker;
    private String temp;
    View myView;

    onChangeListener myCallBack;
    public interface onChangeListener{
        public void eventArrayChange(int m, ArrayList<Event> e);
        public void eventChange(int pos, Event e);
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        myView = inflater.inflate(R.layout.ledger_layout, container, false);

        markerEvents = ((MapsActivity)getActivity()).getCurrentMarker();
        pos = ((MapsActivity)getActivity()).getMarker();
        thisMarker = ((MapsActivity)getActivity()).getTheMarker();
        temp = ((MapsActivity)getActivity()).getTemp();
        temp = temp.replace("~", "");

        EditText location = (EditText) myView.findViewById(R.id.locationLbl);
        location.setText(thisMarker.getTitle());

        if(temp.length() != 0) {
            TextView tempText = (TextView) myView.findViewById(R.id.tempLbl);
            tempText.setText(temp);
        }

        populateListView();

        myCallBack = (onChangeListener) getActivity();

        final ImageButton addButton = (ImageButton) myView.findViewById(R.id.addButton);
        addButton.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                markerEvents.add(new Event("NewEvent", "", ""));
                myCallBack.eventArrayChange(pos, markerEvents);
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
            super(getActivity(), android.R.layout.simple_list_item_1, markerEvents);
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

            button.setText(markerEvents.get(position).getName());
            button.setOnClickListener(new View.OnClickListener() {
                public void onClick(View v) {
                    ((MapsActivity)getActivity()).setInEvent(true);
                    ListView parent = (ListView) v.getParent();
                    final int posn = parent.getPositionForView(v);
                    myCallBack.eventChange(posn, markerEvents.get(posn));
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

    @Override
    public void onDestroyView() {
        EditText location = (EditText) myView.findViewById(R.id.locationLbl);
        thisMarker.setTitle(location.getText().toString());
        super.onDestroyView();
    }
}