# craywatch_selection
 To select localities in GoogleMaps. 
 Open the map in MyMaps by opening Locaties_SWO.GMAP in ./data/input/ 
 - Latitude & Longitude: coordinates
 - isReserved field for point markers

 Proceed by doing one of the following
   1. Change status of point in updateRes - add vrijwillID
   2. Add point - add SugbyINBO; vrijwillID; SampleDate
 
 Open & run the script in ./src/Select_Localities.R
   - update the province, coordinate, gemeente en postkanton, locID fields
   - update WVLC & VHAG

Automated saving of ./data/output/localities.csv



 
