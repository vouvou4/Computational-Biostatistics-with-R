#!/bin/bash

#Creating a loop in linux sh terminal to run my driver script for all .csv files
#Here creating the loop, where i represents the loop variable
#next the the loop variable is being assigned to each file in directory
#I am telling the loop here to loop over each file that starts with COVID19.2020. and ends with csv
#Note that * is a wildcard and tells the shell to find all files that end with csv because it is placed in front of csv
#next do part of code tells loop what to do, here we are running our driver script using Rscript command
#note $ is needed to tell the shell which file to use
#in this case using loop variable ($i) - (sequentially goes through each file)
#The terminal will run the Rscript for each filename sequentially in the directory and give output for all COVID19.2020.*csv files

for i in COVID19.2020.*csv
do
  Rscript process.Covid.R $i
done
