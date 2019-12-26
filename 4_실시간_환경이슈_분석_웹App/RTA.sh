#!/bin/bash

cd /media/Realtime_Analysis

python ./NN_KF_RTA.py
python ./ME_KF_RTA.py
python ./KGF_RTA.py
python ./NN_DBSCAN_RTA.py
python ./ME_DBSCAN_RTA.py
python ./KCO_RTA.py
Rscript ./RTA_AA.R

cd /media/Data/Realtime_Analysis
chmod -R 755 .
chown -R dyjin:dyjin .
