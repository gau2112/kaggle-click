#!bin/bash
# Program:
#    learning count data in 9 days
#    software form H2O
# author: whale
# time : 2014-12-28
# usage: sh countlearning.sh 
daylist="21 22 23 24 25 26 27 28 29"
#day=21
for day in $daylist; do
    echo '############################################################'
    echo 'train data in day:'
    echo $day
    echo '############################################################'
    java -Xmx16G -jar /home/whale/Downloads/h2o-2.9.0.1601/h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision &
    sleep 15s
    echo '############################################################'
    echo 'making data...'
    #Rscript count1.R $day
    Rscript count2.R $day
    echo '############################################################'
    echo 'training...'
    #Rscript learn1.R $day
    Rscript learn2.R $day
    #Rscript testh2o.R
    kill %%
    echo "one day's training is over..."
done

daylist="21 22 23 24 25 26 27 28 29"
#testdaylist="27 28 29 30"
testdaylist="31"
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
        Rscript learntest1.R $testday $day
        #Rscript learntest2.R $testday $day
        #Rscript testh2o.R
        kill %%
        echo "one day's testing is over..."
    done
done

