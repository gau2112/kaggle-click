#!bin/bash
# Program:
#    train libfm with different std
# author: whale
# time : 2015-01-12
# usage: 
# sh xxx.sh

stdlist="0.02 0.03 0.04 0.06 0.07 0.08"

for std in $stdlist; do
    echo '############################################################'
    echo 'std:'
    echo $std
    echo '############################################################'
    /home/whale/Documents/libfm-1.42.src/bin/libFM -cache_size 12000000000 -task c -train train9days -verbosity 1 -test train10thday -iter 8 -dim '1,1,8' -init_stdev $std
done
