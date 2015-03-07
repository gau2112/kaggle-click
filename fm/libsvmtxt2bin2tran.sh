#!bin/bash
# Program:
#    libfm: convert data to binary data and transpose them
# author: whale
# time : 2015-01-12
# usage: 
# sh x.sh

# training data
/home/whale/Documents/libfm-1.42.src/bin/convert --ifile train9days.txt --ofilex train9days.x --ofiley train9days.y
/home/whale/Documents/libfm-1.42.src/bin/transpose --ifile train9days.x --ofile train9days.xt --cache_size 17000000000

# validation data
/home/whale/Documents/libfm-1.42.src/bin/convert --ifile train10thday.txt --ofilex train10thday.x --ofiley train10thday.y
/home/whale/Documents/libfm-1.42.src/bin/transpose --ifile train10thday.x --ofile train10thday.xt --cache_size 17000000000

# test data
/home/whale/Documents/libfm-1.42.src/bin/convert --ifile test.txt --ofilex test.x --ofiley test.y
/home/whale/Documents/libfm-1.42.src/bin/transpose --ifile test.x --ofile test.xt --cache_size 17000000000

