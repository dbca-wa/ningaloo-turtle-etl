# Turtle Tracker
This application maps, tallies and plots turtle track counts, using live data from
the ODK Collect form "TrackCounts 0.10".

## Data flow
* Data is collected using ODK Collect with the form "TrackCounts 0.10".
* Data is uploaded via WiFi to the data clearinghouse 
  [dpaw-data.appspot.com](https://dpaw-data.appspot.com/).
* dpaw-data streams the data automatically to a Google Fusion Table.
* This app loads all data from the Google Fusion Table whenever it is accessed.
