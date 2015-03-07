#!bin/bash
# Program:
#    test learning curve
# author: whale
# time : 2015-01-01
# usage: sh countrand.sh 

java -Xmx16G -jar /home/whale/Downloads/h2o-2.9.0.1601/h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision &
sleep 15s

echo 'making data...'
#Rscript count1.R $day
Rscript count1+.R

randlist="0.9 0.8 0.7 0.6 0.5 0.4"
for rand in $randlist; do
    echo '############################################################'
    Rscript learn2+.R $rand
    echo '############################################################'
done

kill %%
echo '############################################################'

java -Xmx16G -jar /home/whale/Downloads/h2o-2.9.0.1601/h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision &
sleep 15s
echo 'making data...'
#Rscript count1.R $day
Rscript count1.R 1 3
echo 'training...'
#Rscript learn1.R $day
Rscript learn2.R 1 3
#Rscript testh2o.R
kill %%
echo '############################################################'

java -Xmx16G -jar /home/whale/Downloads/h2o-2.9.0.1601/h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision &
sleep 15s
echo 'making data...'
#Rscript count1.R $day
Rscript count1.R 3 1
echo 'training...'
#Rscript learn1.R $day
Rscript learn2.R 3 1
#Rscript testh2o.R
kill %%
echo '############################################################'

java -Xmx16G -jar /home/whale/Downloads/h2o-2.9.0.1601/h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision &
sleep 15s
echo 'making data...'
#Rscript count1.R $day
Rscript count1.R 3 3
echo 'training...'
#Rscript learn1.R $day
Rscript learn2.R 3 3
#Rscript testh2o.R
kill %%
echo '############################################################'
