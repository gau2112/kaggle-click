#!bin/bash
# Program:
#    test m and l parameters
# author: whale
# time : 2014-12-31
# usage: sh countml.sh 

java -Xmx16G -jar /home/whale/Downloads/h2o-2.9.0.1601/h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision &
sleep 15s
echo 'making data...'
#Rscript count1.R $day
Rscript count1.R 1 1
echo 'training...'
#Rscript learn1.R $day
Rscript learn2.R 1 1
#Rscript testh2o.R
kill %%
echo '############################################################'

java -Xmx16G -jar /home/whale/Downloads/h2o-2.9.0.1601/h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision &
sleep 15s
echo 'making data...'
#Rscript count1.R $day
Rscript count1.R 1 2
echo 'training...'
#Rscript learn1.R $day
Rscript learn2.R 1 2
#Rscript testh2o.R
kill %%
echo '############################################################'

java -Xmx16G -jar /home/whale/Downloads/h2o-2.9.0.1601/h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision &
sleep 15s
echo 'making data...'
#Rscript count1.R $day
Rscript count1.R 2 1
echo 'training...'
#Rscript learn1.R $day
Rscript learn2.R 2 1
#Rscript testh2o.R
kill %%
echo '############################################################'

java -Xmx16G -jar /home/whale/Downloads/h2o-2.9.0.1601/h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision &
sleep 15s
echo 'making data...'
#Rscript count1.R $day
Rscript count1.R 2 2
echo 'training...'
#Rscript learn1.R $day
Rscript learn2.R 2 2
#Rscript testh2o.R
kill %%
echo '############################################################'

java -Xmx16G -jar /home/whale/Downloads/h2o-2.9.0.1601/h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision &
sleep 15s
echo 'making data...'
#Rscript count1.R $day
Rscript count1.R 3 2
echo 'training...'
#Rscript learn1.R $day
Rscript learn2.R 3 2
#Rscript testh2o.R
kill %%
echo '############################################################'

java -Xmx16G -jar /home/whale/Downloads/h2o-2.9.0.1601/h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision &
sleep 15s
echo 'making data...'
#Rscript count1.R $day
Rscript count1.R 2 3
echo 'training...'
#Rscript learn1.R $day
Rscript learn2.R 2 3
#Rscript testh2o.R
kill %%
echo '############################################################'
