# Traffic

The goal of this project was to find the most dangerous intersections in Manhattan and try to make some nice looking visualizations.  The data that I use, **NYPD_Motor_Vehicle_Collisions.csv**, documents all collisions in New York City between July 2012 and January 2015.  It gives the location of each accident in the form of latitude and longitude and also indicates if either a pedestrian or cyclist was affected.  I estimate the the danger level, defined by accidents per unit time, at every location using kernel density estimation.  The highest peaks of the estimated density will reveal the most dangerous intersections.

**findDanger.R** finds the latitude and longitude of the five to ten most dangerous intersections split by upper and lower Manhattan and all accidents, accidents involving a pedestrian, and accidents involving a cyclist.  In all, six sets of coordinates are produced.  These are found in the **coords** folder.

**dangerVis.R** produces visualizations.  I put longitude on the horizontal axis, latitude on the vertical and plot every collision.  There are enough accidents that the shape of Manhattan is clearly discernible.  I then overlay the estimated danger level.  I use a color gradient or heat map to indicate areas of more or less danger, so more dangerous areas are redder in color.  Finally, I overlay a small number of large blue dots that correspond to the most dangerous points on the map.  I again split Manhattan into upper and lower and have separate maps for all accidents, those involving pedestrians, and those involving cyclists.  These can be found in the **viz** folder.

Read **Manhattan_Traffic.pdf** for a summary of methodology and results.
