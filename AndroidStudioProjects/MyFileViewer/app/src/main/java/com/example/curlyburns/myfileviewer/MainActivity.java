package com.example.curlyburns.myfileviewer;

import android.content.Intent;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public class MainActivity extends AppCompatActivity {

    FileInfo[] files = {
            new FileInfo("hello.txt", "9/16/2016", "1KB"),
            new FileInfo("haiku.txt", "9/2/2016", "1KB")
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        populateListView();
        registerListViewClicks();

    }

    private void populateListView() {

        //Create Adapter
        ArrayAdapter<FileInfo> myListAdapter = new myArrayAdapter<FileInfo>();

        //set Adapter
        ListView myList = (ListView)findViewById(R.id.listView);
        myList.setAdapter(myListAdapter);

    }

    private class myArrayAdapter<T> extends ArrayAdapter<FileInfo> {
        public myArrayAdapter() {
            super(MainActivity.this, R.layout.my_layout, files);
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent){
            View itemView = convertView;
            if(itemView == null)
                //check if not recycled
                itemView = getLayoutInflater().inflate(R.layout.my_layout, parent, false);

            //creat aFile to work with
            FileInfo aFile = files[position];

            TextView fileName = (TextView) itemView.findViewById(R.id.fileName);
            fileName.setText(aFile.getName());
            TextView date = (TextView) itemView.findViewById(R.id.date);
            date.setText("Last Modified: " + aFile.getDate());
            TextView size = (TextView) itemView.findViewById(R.id.size);
            size.setText("Size: " + aFile.getSize());


            return itemView;
        }
    }

    private void registerListViewClicks(){
        ListView myGrid = (ListView)findViewById(R.id.listView);
        myGrid.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                //create aFile to work with
                FileInfo aFile = files[position];

                //check for bad stuffs
                try {
                    readFile(aFile.getName());
                } catch (IOException e) {
                    Toast.makeText(getApplicationContext(),
                            "Error: " + e.getMessage(), Toast.LENGTH_LONG).show();
                }
            }
        });
    }

    public void readFile(String name) throws IOException {
        //strip extension from file name
        String[] strippedName = name.split("\\.");

        //read file
        String str="";
        StringBuffer buff = new StringBuffer();
        InputStream is = getResources().openRawResource(
                getResources().getIdentifier(strippedName[0],
                        "raw", getPackageName()));
        BufferedReader reader = new BufferedReader(new InputStreamReader(is));
        if (is != null) {
            while ((str = reader.readLine()) != null) {
                buff.append(str + "\n" );
            }
        }
        is.close();

        //create Intent and add what we need
        Intent intent = new Intent(MainActivity.this, com.example.curlyburns.myfileviewer.SecondActivity.class);
        intent.putExtra("Contents", buff.toString());
        startActivity(intent);

    }

}
