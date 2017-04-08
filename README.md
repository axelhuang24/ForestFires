# ForestFires
We are going to study a set of data used to predict the total burnt area of forest fires. According to specialists in metearology, 
the 4 main variables in weather for this case is wind, relative humidity, temperature and rain. Other variables in this dataset that may 
help in predicting the area are the coordinates (x and y), the month and the day. And finally we have 3 codes and an index. We will do a 
brief explanation of these 4. The 3 codes are FFMC (Fine Fuel Moisture Code), DMC (Duff Moisture Code) and DC (Drought Code). The FFMC 
is about ignition and spread of fire and is obtained by using all the 4 meteorological variables presented before. The DMC and the DC 
affects the fire intensity. DMC is obtained with rain, relative humidity and temperature and DC with rain and temperature. 
The ISI (Initial Spread Index) denotes the velocity spread of the fire and is calculated using the FFMC and the wind. These 4 were kept 
in the dataset since they were all directly affected by the 4 weather variables.Therefore the BUI (BuildUp Index) which is calculated 
using DMC and DC and the FWI (Fire Weather Index) calculated using ISI and BUI were omitted.Although the SVM (Support Vector Machine) is 
the best method to predict the area according to studies, we are going to do a multiple reggression study with this dataset to predict the
area of burnt forest at first. And them, if we can't get an ideal model, we will try the SVM.
