#!bin/bash
# Program:
#    split data to 10 separate days
# author: whale
# time : 2014-11-26
# usage: time sh splitdays.sh 
daylist="21 22 23 24 25 26 27 28 29 30"
#
for day in $daylist; do
    head -n 1 train.csv > train_day$day.csv
    grep ",1410"$day"[0-9][0-9]," train.csv >> train_day$day.csv
done

