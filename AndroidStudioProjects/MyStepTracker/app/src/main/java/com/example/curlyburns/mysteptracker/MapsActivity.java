package com.example.curlyburns.mysteptracker;

import android.Manifest;
import android.content.pm.PackageManager;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.location.Criteria;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Build;
import android.support.annotation.NonNull;
import android.support.v4.app.ActivityCompat;
import android.support.v4.app.FragmentActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewConfiguration;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.OnMapReadyCallback;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.Polyline;
import com.google.android.gms.maps.model.PolylineOptions;

import java.util.ArrayList;

public class MapsActivity extends FragmentActivity implements OnMapReadyCallback {

    private GoogleMap mMap;
    private LocationManager manager;
    private LatLng me;
    private Location myLocation;//current user LatLng (as a Location)
    private Marker currentMarker;//current user location
    private Marker startMarker;//beginning of route
    private Polyline line;//tracking line
    private ArrayList<LatLng> linePoints;//used to draw line
    private boolean tracking;//we tracking?

    private long lastClick;// for calculating double click
    private int steps;//number of steps
    private boolean counting;//we counting steps?

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
     * This is where we can add markers or lines, add listeners or move the camera. In this case,
     * we just add a marker near Sydney, Australia.
     * If Google Play services is not installed on the device, the user will be prompted to install
     * it inside the SupportMapFragment. This method will only be triggered once the user has
     * installed Google Play services and returned to the app.
     */
    @Override
    public void onMapReady(GoogleMap googleMap) {
        mMap = googleMap;

        if(ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED
                && ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED)
            requestPermissions(new String[]{Manifest.permission.ACCESS_COARSE_LOCATION,
                    Manifest.permission.ACCESS_FINE_LOCATION}, 10);

        mMap.setOnMarkerClickListener(new GoogleMap.OnMarkerClickListener(){
            @Override
            public boolean onMarkerClick(Marker marker) {
                return true; //"disables" marker clicks
            }
        });
        mMap.setOnMyLocationButtonClickListener(new GoogleMap.OnMyLocationButtonClickListener() {
            @Override
            public boolean onMyLocationButtonClick() {
                TextView trackText = (TextView) findViewById(R.id.tracking);
                if(!counting) {
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
                }
                return false;
            }
        });

        RelativeLayout myLayout = (RelativeLayout) findViewById(R.id.main_activity_layout);
        myLayout.setOnTouchListener(new View.OnTouchListener() {
            @Override
            public boolean onTouch(View v, MotionEvent event) {
                long thisClick = System.currentTimeMillis();
                TextView instr = (TextView) findViewById(R.id.instr);
                TextView trackText = (TextView) findViewById(R.id.tracking);
                TextView countText = (TextView) findViewById(R.id.counting);
                TextView stepText = (TextView) findViewById(R.id.steps);

                if(thisClick - lastClick < ViewConfiguration.getDoubleTapTimeout()){
                    Log.d("Map", "it double clicked");
                    if(counting){
                        counting = false;
                        stepText.setText(steps + " steps on route");
                        countText.setText("");
                        instr.setText("Double tap here to start step counter");
                    }
                    else {
                        if(startMarker != null)
                            startMarker.remove();
                        if(line != null)
                            line.remove();
                        linePoints.clear();
                        startMarker = mMap.addMarker(new MarkerOptions().position(currentMarker.getPosition()));
                        mMap.getUiSettings().setZoomGesturesEnabled(false);
                        mMap.getUiSettings().setScrollGesturesEnabled(false);
                        counting = true;
                        tracking = true;
                        steps = 0;
                        countText.setText("Counting");
                        trackText.setText("Tracking");
                        stepText.setText("");
                        instr.setText("Double tap here to stop step counter");
                    }
                }
                else
                    lastClick = thisClick;
                return false;
            }
        });

        myLocation = null;
        lastClick = -1;
        steps = 0;
        counting = false;
        tracking = false;
        startMarker = null;
        line = null;
        linePoints = new ArrayList<>();

        SensorManager mSensorManager = (SensorManager) getSystemService(SENSOR_SERVICE);
        Sensor stepCounter = mSensorManager.getDefaultSensor(Sensor.TYPE_STEP_COUNTER);
        MySensorListener sensorListener = new MySensorListener();
        mSensorManager.registerListener(sensorListener, stepCounter, SensorManager.SENSOR_DELAY_NORMAL);

        manager = (LocationManager) getSystemService(LOCATION_SERVICE);
        LocationUpdateListener locationListener = new LocationUpdateListener();
        Criteria criteria = new Criteria();
        String provider = manager.getBestProvider(criteria, true);

            mMap.setMyLocationEnabled(true);
            Toast theToast = Toast.makeText(MapsActivity.this, "Use this button to toggle tracking. ->", Toast.LENGTH_LONG);
            theToast.setGravity(Gravity.TOP, 0, 0);
            theToast.show();

            myLocation = manager.getLastKnownLocation(LocationManager.GPS_PROVIDER);
            me = new LatLng(myLocation.getLatitude(), myLocation.getLongitude());
            mMap.moveCamera(CameraUpdateFactory.newLatLngZoom(me, 20));
            currentMarker = mMap.addMarker(new MarkerOptions().position(me)
                .icon(BitmapDescriptorFactory.defaultMarker(BitmapDescriptorFactory.HUE_CYAN)));
            manager.requestLocationUpdates(provider, 0, 0, locationListener);
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
        }
    }

    class LocationUpdateListener implements LocationListener{
        private LocationUpdateListener() {
            super();
        }

        @Override
        public void onLocationChanged(Location location) {
            if(ActivityCompat.checkSelfPermission(MapsActivity.this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED
                    && ActivityCompat.checkSelfPermission(MapsActivity.this, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED)
                requestPermissions(new String[]{Manifest.permission.ACCESS_COARSE_LOCATION,
                        Manifest.permission.ACCESS_FINE_LOCATION}, 10);

                myLocation = manager.getLastKnownLocation(LocationManager.GPS_PROVIDER);
                me = new LatLng(myLocation.getLatitude(), myLocation.getLongitude());
                currentMarker.setPosition(me);
                if(tracking) {
                    if (counting) {
                        if (line == null)
                            line = mMap.addPolyline(new PolylineOptions().clickable(false).width(5));
                        linePoints.add(me);
                        line.setPoints(linePoints);
                    }
                    mMap.animateCamera(CameraUpdateFactory.newLatLngZoom(me, 20));
                }
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
    }

    class MySensorListener implements SensorEventListener{

        @Override
        public void onSensorChanged(SensorEvent event) {
            steps++;
        }

        @Override
        public void onAccuracyChanged(Sensor sensor, int accuracy) {

        }
    }

}
