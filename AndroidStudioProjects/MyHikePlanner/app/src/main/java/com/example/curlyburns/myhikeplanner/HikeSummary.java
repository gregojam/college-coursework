package com.example.curlyburns.myhikeplanner;

import android.app.Fragment;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;

import com.google.android.gms.maps.model.Marker;

import java.util.ArrayList;

/**
 * Created by CurlyBurns on 10/15/2016.
 */

public class HikeSummary extends Fragment {

    private ArrayList<Marker> within10;
    private ArrayList<Marker> allMarkers;
    private ArrayList<String> theTemps;
    View myView;

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        myView = inflater.inflate(R.layout.hike_summary_layout, container, false);

        within10 = ((MapsActivity)getActivity()).getWithin10();
        allMarkers = ((MapsActivity)getActivity()).getAllMarkers();
        theTemps = ((MapsActivity)getActivity()).getTheTemps();

        populateTheLists();

        return myView;
    }

    private void populateTheLists() {

        //Create Adapters
        MyCurrentAdapter<Marker> myCurrentAdapter = new MyCurrentAdapter<>();
        MyPrevAdapter<Marker> myPrevAdapter = new MyPrevAdapter<>();
        MyFutureAdapter<Marker> myFutureAdapter = new MyFutureAdapter<>();

        //set Adapter
        ListView myCurrentList = (ListView) myView.findViewById(R.id.currentVisit);
        myCurrentList.setAdapter(myCurrentAdapter);

        ListView myPrevList = (ListView) myView.findViewById(R.id.haveVisit);
        myPrevList.setAdapter(myPrevAdapter);

        ListView myFutureList = (ListView) myView.findViewById(R.id.needVisit);
        myFutureList.setAdapter(myFutureAdapter);
    }

    private class MyCurrentAdapter<T> extends ArrayAdapter<Marker> {
        public MyCurrentAdapter() {
            super(getActivity(), R.layout.a_location_layout, within10);
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent){
            View itemView = convertView;
            if(itemView == null)
                //check if not recycled
                itemView = getActivity().getLayoutInflater().inflate(R.layout.a_location_layout, parent, false);

            TextView name = (TextView) itemView.findViewById(R.id.thisLocationName);
            name.setText(within10.get(position).getTitle());
            TextView temp = (TextView) itemView.findViewById(R.id.thisLocationTemp);
            temp.setText(theTemps.get(allMarkers.indexOf(within10.get(position))));


            return itemView;
        }
    }

    private class MyPrevAdapter<T> extends ArrayAdapter<Marker> {
        public MyPrevAdapter() {
            super(getActivity(), R.layout.a_location_layout, allMarkers);
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent){
            View itemView = convertView;
            if(itemView == null)
                //check if not recycled
                itemView = getActivity().getLayoutInflater().inflate(R.layout.a_location_layout, parent, false);

            TextView name = (TextView) itemView.findViewById(R.id.thisLocationName);
            name.setText(allMarkers.get(position).getTitle());
            TextView temp = (TextView) itemView.findViewById(R.id.thisLocationTemp);
            temp.setText(theTemps.get(position));

            if(!within10.contains(allMarkers.get(position)) && theTemps.get(position).length() != 0)
                return itemView;
            return null;
        }
    }

    private class MyFutureAdapter<T> extends ArrayAdapter<Marker> {
        public MyFutureAdapter() {
            super(getActivity(), R.layout.a_location_layout, allMarkers);
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent){
            View itemView = convertView;
            if(itemView == null)
                //check if not recycled
                itemView = getActivity().getLayoutInflater().inflate(R.layout.a_location_layout, parent, false);

            TextView name = (TextView) itemView.findViewById(R.id.thisLocationName);
            name.setText(allMarkers.get(position).getTitle());
            TextView temp = (TextView) itemView.findViewById(R.id.thisLocationTemp);
            temp.setText(theTemps.get(position));

            if(!within10.contains(allMarkers.get(position)) && theTemps.get(position).length() == 0)
                return itemView;
            return null;
        }
    }
}
