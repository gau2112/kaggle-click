#!bin/bash
# Program:
#    test count data in 9 days
# author: whale
# time : 2014-12-20
# usage: sh counttest.sh 
daylist="21 22 23 24 25 26 27 28 29"
testdaylist="30 31"
#testdaylist="29"
#day=21
for testday in $testdaylist; do
    echo '############################################################'
    echo 'test data in day:'
    echo $testday
    echo '############################################################'
    
    for day in $daylist; do
        echo '############################################################'
        echo 'models from day:'
        echo $day
        java -Xmx16G -jar /home/whale/Downloads/h2o-2.9.0.1601/h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision &
        sleep 15s
        echo '############################################################'
        echo 'making data...'
        Rscript counttest1.R $testday $day
        echo '############################################################'
        echo 'testing...'
        #Rscript learntest1.R $testday $day
        Rscript learntest2.R $testday $day
        #Rscript testh2o.R
        kill %%
        echo "one day's testing is over..."
    done
done

