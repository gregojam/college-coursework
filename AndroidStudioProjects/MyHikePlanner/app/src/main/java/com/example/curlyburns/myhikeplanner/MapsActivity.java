package com.example.curlyburns.myhikeplanner;

import android.Manifest;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.content.pm.PackageManager;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.location.Criteria;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.CountDownTimer;
import android.os.StrictMode;
import android.os.Vibrator;
import android.support.annotation.NonNull;
import android.support.v4.app.ActivityCompat;
import android.support.v4.app.FragmentActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.View;
import android.widget.Button;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.OnMapReadyCallback;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.maps.android.SphericalUtil;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;

public class MapsActivity extends FragmentActivity implements OnMapReadyCallback,
        FragmentLedger.onChangeListener, FragmentEvent.onChangeListener{

    private GoogleMap mMap;
    private LocationManager manager;
    private LatLng me;//current user LatLng
    private Location myLocation;//current user Location
    private boolean tracking;//we tracking?
    private int markerNum;

    //These ArrayLists must stay in sync with one another!
    private ArrayList<Marker> theMarkers;
    private ArrayList<String> theTemps;
    private ArrayList<ArrayList> markerEvents;
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    private ArrayList<Event> currentMarker;
    private Event currentEvent;
    private int thisMarker;//index for theMarkers, theTemps,and markerEvents
    private int event;//index for currentMarker

    private boolean inEvent;//we in an Event?
    private int backCheck;//number of back button attempts in Event

    private int tempr;//current temperature

    private boolean hasVibed;
    private boolean within15;
    private ArrayList<Marker> within10;

    private boolean reset1Set;
    private boolean reset2Set;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_maps);

        // Obtain the SupportMapFragment and get notified when the map is ready to be used.
        SupportMapFragment mapFragment = (SupportMapFragment) getSupportFragmentManager()
                .findFragmentById(R.id.map);
        mapFragment.getMapAsync(this);
    }


    /**
     * Manipulates the map once available.
     * This callback is triggered when the map is ready to be used.
     * This is where we can add markers or lines, add listeners or move the camera.
     * If Google Play services is not installed on the device, the user will be prompted to install
     * it inside the SupportMapFragment. This method will only be triggered once the user has
     * installed Google Play services and returned to the app.
     */
    @Override
    public void onMapReady(GoogleMap googleMap) {
        mMap = googleMap;
        myLocation = null;
        tracking = false;
        markerNum = 1;
        theMarkers = new ArrayList<>();
        theTemps = new ArrayList<>();
        markerEvents = new ArrayList<>();
        inEvent = false;
        backCheck = 0;
        tempr = 0;
        hasVibed = false;
        within15 = false;
        within10 = new ArrayList<>();

        readTheMarkers();

        final Vibrator vibe = (Vibrator) getSystemService(VIBRATOR_SERVICE);

        final SensorManager mSensorManager = (android.hardware.SensorManager) getSystemService(SENSOR_SERVICE);
        final Sensor thermometer = mSensorManager.getDefaultSensor(Sensor.TYPE_AMBIENT_TEMPERATURE);
        final MySensorListener sensorListener = new MySensorListener();
        if(thermometer != null)
            mSensorManager.registerListener(sensorListener, thermometer, SensorManager.SENSOR_DELAY_NORMAL);
        else
            Toast.makeText(this, "No temperature sensor detected. Temperatures disabled.", Toast.LENGTH_LONG).show();

        final FragmentManager fragManager = getFragmentManager();

        // Check for permissions
        if(ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED
                && ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED
                && ActivityCompat.checkSelfPermission(this, Manifest.permission.VIBRATE) != PackageManager.PERMISSION_GRANTED) {

            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.ACCESS_COARSE_LOCATION, //request location
                    Manifest.permission.ACCESS_FINE_LOCATION}, 10);

            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.VIBRATE}, 20); //request vibrate
        }

        // Map click adds a marker
        mMap.setOnMapClickListener(new GoogleMap.OnMapClickListener(){
            @Override
            public void onMapClick(LatLng latLng) {
                theMarkers.add(mMap.addMarker(new MarkerOptions().position(latLng).title("" + markerNum)));
                theTemps.add("");
                markerEvents.add(new ArrayList<Event>());
                markerNum++;
            }
        });

        // Marker click opens that marker's agenda
        mMap.setOnMarkerClickListener(new GoogleMap.OnMarkerClickListener(){
            @Override
            public boolean onMarkerClick(Marker marker) {
                thisMarker = theMarkers.indexOf(marker);
                currentMarker = markerEvents.get(thisMarker);
                FragmentTransaction fragTrans = fragManager.beginTransaction();
                FragmentLedger aLedger = new FragmentLedger();
                fragTrans.replace(R.id.main_activity_layout, aLedger);
                fragTrans.addToBackStack(null);
                fragTrans.commit();
                return true; //disables default handling
            }
        });

        // Track user and disable zoom and scroll. (toggle)
        mMap.setOnMyLocationButtonClickListener(new GoogleMap.OnMyLocationButtonClickListener() {
            @Override
            public boolean onMyLocationButtonClick() {
                TextView trackText = (TextView) findViewById(R.id.tracking);
                    tracking = !tracking;
                    if (tracking) {
                        mMap.getUiSettings().setZoomGesturesEnabled(false);
                        mMap.getUiSettings().setScrollGesturesEnabled(false);
                        trackText.setText("Tracking");
                    }
                    else{
                        mMap.getUiSettings().setZoomGesturesEnabled(true);
                        mMap.getUiSettings().setScrollGesturesEnabled(true);
                        trackText.setText("");
                    }
                return false;
            }
        });

        Button summaryBtn = (Button) findViewById(R.id.summaryBtn);
        summaryBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                FragmentTransaction fragTrans = fragManager.beginTransaction();
                HikeSummary summary = new HikeSummary();
                fragTrans.replace(R.id.main_activity_layout, summary);
                fragTrans.addToBackStack(null);
                fragTrans.commit();
            }
        });

        Button reset1 = (Button) findViewById(R.id.reset1);
        reset1.setOnTouchListener(new View.OnTouchListener() {
            @Override
            public boolean onTouch(View v, MotionEvent event) {
                switch(event.getAction()){
                    case MotionEvent.ACTION_DOWN:
                        reset1Set = true;
                        if(reset2Set){
                            markerNum = 1;
                            for(Marker mark : theMarkers){
                                mark.remove();
                            }
                            theMarkers.clear();
                            theTemps.clear();
                            markerEvents.clear();
                        }
                        break;

                    case MotionEvent.ACTION_UP:
                        reset1Set = false;
                        break;
                }
                return false;
            }
        });

        Button reset2 = (Button) findViewById(R.id.reset2);
        reset2.setOnTouchListener(new View.OnTouchListener() {
            @Override
            public boolean onTouch(View v, MotionEvent event) {
                switch(event.getAction()){
                    case MotionEvent.ACTION_DOWN:
                        reset2Set = true;
                        if(reset1Set){
                            markerNum = 1;
                            for(Marker mark : theMarkers){
                                mark.remove();
                            }
                            theMarkers.clear();
                            theTemps.clear();
                            markerEvents.clear();
                        }
                        break;

                    case MotionEvent.ACTION_UP:
                        reset2Set = false;
                        break;
                }
                return false;
            }
        });

        // Show My Location button and prompt how to use.
        mMap.setMyLocationEnabled(true);
        Toast theToast = Toast.makeText(MapsActivity.this, "Use this button to toggle tracking. ->", Toast.LENGTH_LONG);
        theToast.setGravity(Gravity.TOP, 0, 0);
        theToast.show();

        Toast.makeText(MapsActivity.this, "Press both 'reset' buttons at the same time to clear the map.",
                Toast.LENGTH_LONG).show();

        // Set up location manager and location update listener
        manager = (LocationManager) getSystemService(LOCATION_SERVICE);
        String provider = manager.getBestProvider(new Criteria(), true);
        manager.requestLocationUpdates(provider, 0, 0, new LocationListener() {
            @Override
            public void onLocationChanged(Location location) {
                // update user location
                myLocation = manager.getLastKnownLocation(LocationManager.GPS_PROVIDER);
                me = new LatLng(myLocation.getLatitude(), myLocation.getLongitude());

                // check distance from markers
                for (Marker place : theMarkers) {
                    double dis = SphericalUtil.computeDistanceBetween(me, place.getPosition());

                    // vibrate once if within 10m of any marker
                    if (dis <= 10) {
                        if(!within10.contains(place))
                            within10.add(place);
                        within15 = true;
                        if(!hasVibed) {
                            vibe.vibrate(1000);
                            hasVibed = true;
                        }

                        // associate temp with marker if recorded
                        if(thermometer != null)
                            theTemps.set(theMarkers.indexOf(place), "" + tempr);
                        else
                            theTemps.set(theMarkers.indexOf(place), "~");

                    }
                    // don't let vibrate reset until more than 15m away.
                    else if(dis <= 15 && dis > 10){
                        within10.remove(place);
                        within15 = true;

                    }
                    else
                        within10.remove(place);
                }

                if(within10.isEmpty()){
                    // hide summary button
                    ((Button) findViewById(R.id.summaryBtn)).setClickable(false);
                    ((Button) findViewById(R.id.summaryBtn)).setVisibility(View.INVISIBLE);
                }
                else{
                    // show summary button
                    ((Button) findViewById(R.id.summaryBtn)).setClickable(true);
                    ((Button) findViewById(R.id.summaryBtn)).setVisibility(View.VISIBLE);
                }

                // reset vibrate if more than 15m from all markers
                hasVibed = within15;
                within15 = false;

                // animate camera to user if tracking
                if(tracking)
                    mMap.animateCamera(CameraUpdateFactory.newLatLngZoom(me, 20));

            }

            @Override
            public void onStatusChanged(String provider, int status, Bundle extras) {

            }

            @Override
            public void onProviderEnabled(String provider) {

            }

            @Override
            public void onProviderDisabled(String provider) {

            }
        });

        // Let the Location update before moving camera
        CountDownTimer waitATick = new CountDownTimer(2000, 1000){

            @Override
            public void onTick(long millisUntilFinished) {

            }

            @Override
            public void onFinish() {
                //  go to user to start
                myLocation = manager.getLastKnownLocation(LocationManager.GPS_PROVIDER);
                me = new LatLng(myLocation.getLatitude(), myLocation.getLongitude());
                mMap.animateCamera(CameraUpdateFactory.newLatLngZoom(me, 20),
                        new GoogleMap.CancelableCallback() {
                            @Override
                            public void onFinish() {
                                // allow user interaction
                                RelativeLayout shield = (RelativeLayout)findViewById(R.id.shield);
                                shield.setVisibility(View.GONE);//allow user interaction at this point
                            }

                            @Override
                            public void onCancel() {

                            }
                        });
            }
        };

        waitATick.start();
    }

    public ArrayList<Event> getCurrentMarker() {
        return currentMarker;
    }

    public Marker getTheMarker() { return theMarkers.get(thisMarker); }

    public int getMarker() {
        return thisMarker;
    }

    public String getTemp() { return theTemps.get(thisMarker); }

    public Event getCurrentEvent() {
        return currentEvent;
    }

    public int getEvent() {
        return event;
    }

    public void setInEvent(boolean ifWeAre){ inEvent = ifWeAre; }

    public ArrayList<Marker> getAllMarkers() { return theMarkers; }

    public ArrayList<String> getTheTemps() { return theTemps; }

    public ArrayList<Marker> getWithin10() { return within10; }

    @Override
    public void eventArrayChange(int m, ArrayList<Event> e) {
        markerEvents.set(m, e);
    }

    @Override
    public void eventChange(int pos, Event e){
        event = pos;
        currentEvent = e;
    }

    @Override
    public void onSaveChanges(int m, int ev, String title, String time, String notes){
        inEvent = false;
        backCheck = 0;
        ((Event) markerEvents.get(m).get(ev)).setName(title);
        ((Event) markerEvents.get(m).get(ev)).setTime(time);
        ((Event) markerEvents.get(m).get(ev)).setNotes(notes);
    }

    @Override
    public void deleteEvent(int m, int ev){
        inEvent = false;
        backCheck = 0;
        markerEvents.get(m).remove(ev);
    }

    @Override
    public void cancelChanges(){
        inEvent = false;
        backCheck = 0;
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions,
                                           @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);

        switch (requestCode) {
            case 10:
                if (grantResults.length > 0
                        && grantResults[0] != PackageManager.PERMISSION_GRANTED)
                    Toast.makeText(this, "Location permission denied.", Toast.LENGTH_LONG).show();
                break;

            case 20:
                Toast.makeText(this, "Vibrate permission denied.", Toast.LENGTH_LONG).show();
        }
    }

    public class MySensorListener implements SensorEventListener {

        @Override
        public void onSensorChanged(SensorEvent event) {
            tempr =  (int) event.values[0];
        }

        @Override
        public void onAccuracyChanged(Sensor sensor, int accuracy) {

        }
    }

    // make user use cancel button to exit events
    @Override
    public void onBackPressed() {
        if(!inEvent)
            super.onBackPressed();
        else {
            backCheck++;
            if (backCheck > 2)
                Toast.makeText(this, "'Save' changes, 'cancel' changes, or 'delete' event to return.", Toast.LENGTH_LONG).show();
        }
    }

    @Override
    protected void onStop() {
        try {
            saveTheMarkers();
        } catch (IOException e) {
            e.printStackTrace();
        }
        super.onStop();
    }

    private void saveTheMarkers() throws IOException {
        StrictMode.ThreadPolicy pol = StrictMode.allowThreadDiskWrites();
        FileOutputStream fos = openFileOutput("MyMarkers.txt", MODE_PRIVATE);
        DataOutputStream dos = new DataOutputStream(fos);
        dos.writeInt(theMarkers.size()); // marker count
        for (Marker marker : theMarkers) {
            thisMarker = theMarkers.indexOf(marker);
            String temperature = theTemps.get(thisMarker);
            currentMarker = markerEvents.get(thisMarker);
            if(temperature.length() == 0)
                temperature = "&";

            String events = "" + currentMarker.size(); // associated event count
            for (Event ev : currentMarker) {
                if(ev.getName().length() == 0)
                    events = events + ",~" + " ";
                else
                    events = events + ",~" + ev.getName();

                if(ev.getTime().length() == 0)
                    events = events + ",~" + " ";
                else
                    events = events + ",~" + ev.getTime();

                if(ev.getNotes().length() == 0)
                    events = events + ",~" + " ";
                else
                    events = events + ",~" + ev.getNotes();
            }

            dos.writeUTF(marker.getPosition().latitude + ",~" + marker.getPosition().longitude +
                    ",~" + marker.getTitle() + ",~" + temperature + ",~" + events);
        }
        dos.close();
        fos.close();
        StrictMode.setThreadPolicy(pol);
    }

    private void readTheMarkers() {
        // allow read
        StrictMode.ThreadPolicy pol = StrictMode.allowThreadDiskReads();
        try {
            FileInputStream fis = openFileInput("MyMarkers.txt");
            DataInputStream dis = new DataInputStream(fis);
            int count = dis.readInt(); // number of markers
            markerNum = count;
            int eventCount; // number of events for particular marker
            for (int i = 0; i < count; i++) {
                String inputStr = dis.readUTF();
                String[] info = inputStr.split(",~");

                //load marker
                LatLng pos = new LatLng(Double.parseDouble(info[0]), Double.parseDouble(info[1]));
                String name = info[2];
                theMarkers.add(mMap.addMarker(new MarkerOptions().position(pos).title(name)));

                //load temp
                if(info[3].equals("&"))
                    theTemps.add("");
                else
                    theTemps.add(info[3]);

                //loard associated events
                ArrayList<Event> savedEvents = new ArrayList<>();
                eventCount = Integer.parseInt(info[4]);
                for (int j = 0; j < eventCount; j++){
                    String eventName = info[5+j*3];
                    if(eventName.equals(" "))
                        eventName = "";

                    String eventTime =  info[6+j*3];
                    if(eventTime.equals(" "))
                        eventTime = "";

                    String eventNotes = info[7+j*3];
                    if(eventNotes.equals(" "))
                        eventNotes = "";
                    savedEvents.add(new Event(eventName, eventTime, eventNotes));
                }
                markerEvents.add(savedEvents);
            }
            dis.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        // reset thread policy
        StrictMode.setThreadPolicy(pol);
    }
}
