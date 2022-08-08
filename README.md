# powerlinemonsters - the effect of power line construction projects on voting results
This is my master thesis project. I analyze the effect of the 2013 announcement of powerline construction projects on federal (BTW) and state elections (LTW) voting outcomes in German municipalities.

For this, I geocode the endpoints of proposed powerlines given in the 2013 Federal Demand Plan Act (BBPLG). I then construct a shapefile which has the (direct) line between the endpoints and examine, whether these lines intersect with municipalities which gives me my treatment indicator. Using this, I estimate Differences-in-Differences (DiD) Regression models to estimate the effect of powerline construction projects on voting results

## Overview of files
- pl_corridors calculates power lines routes and returns a shape file
- pl_treatment calculate the treatment indicator (powerline intersects with municipality)
- btw_/ltw_data_prep prepare the federal and state election data for analysis
- controls_prep prepares a set of control variables
- btw_/ltw_eda provide exploratory analysis 
- btw_/ltw_did provide the DiD Models

## Note
Raw data cannot be provided due to large file sizes. To allow replication, I plan to add a script that pulls the data from the Genesis/Regionaldatenbank APIs at a later point.