#creating main driver script for assignment 8

#source utilities file
  source("AnalysisUtilities.R")

#Create command line arguments

  args <- commandArgs(trailingOnly = TRUE)

  #Add defensive coding for the command line arguments

  if(length(args) <= 1){
    stop("This script takes two arguments. A file name and either 1, 2, or 3.")
  }

  if(!file.exists(args[1])){
    stop("This file does not exist. Please enter the correct filename: merged.haplogroup.file.csv")
  }

  if(length(args) >2){
    stop("Too many arguments presented. This script only takes two arguments, a csv file and either 1, 2, or 3.")
  }

#start funning the script
#use cat to tell the user which file we are processing

  cat("Processing file", args[1], "...", "\n")
  cat("--------------", "\n")

#Use utility file function for data frame creation and assign to variable
#this will have the cleaned data frame and will be used to select the columns of interest

  haplogroup.data.frame <- create.data.frame("merged.haplogroup.file.csv")

#Assign all the columns I will analyze to variables which I can call later in the various
#if statements as needed

  h <- haplogroup.data.frame[, "H.HV.V"]
  j <- haplogroup.data.frame[, "J.T"]
  u <- haplogroup.data.frame[, "U.k."]
  ftnd <- haplogroup.data.frame[, "FTND_score"]

#now create chain of if statements, for the various arguments to complete specific analyses
#Here for each argument they complete the same analyses but for different haplogroups of interest
#the dependent variable is always ftnd score, but the explanatory variables (haplogroups) they can vary
#Each argument computes the correlation estimator for those two variables, runs the anova for all three haplogroups
#for each argument as well due to the nature of the anova
#then compute logistic regression because of binomial data, for the specific haplogroup and ftnd FTND_score
#also creates a graph for this logistic regression
#Lastly runs the kNN algorithm which similarly as the anova uses all the haplogroups to predict FTND_score and prints
#out the accuracy of this algorithm


  if(args[2]=="1"){

    cat("Computing correlation indicators for H.HV.V haplogroup:", "\n")
    correlation.estimators(h, ftnd)
    cat("--------------", "\n")
    anova.analysis(h, j, u, ftnd)
    cat("--------------", "\n")
    cat("Computing logistic regression for H.HV.V haplogroup:", "\n")
    log.regression <- logistic.regression(h, ftnd)
    logistic.graph(h, ftnd, log.regression)
    cat("--------------", "\n")
    knn.analysis(haplogroup.data.frame, c(5, 6, 7), 8)

  }else if(args[2]=="2"){

    cat("Computing correlation indicators for J.T haplogroup:", "\n")
    correlation.estimators(j, ftnd)
    cat("--------------", "\n")
    anova.analysis(h, j, u, ftnd)
    cat("--------------", "\n")
    cat("Computing logistic regression for J.T haplogroup:", "\n")
    log.regression <- logistic.regression(j, ftnd)
    logistic.graph(j, ftnd, log.regression)
    cat("--------------", "\n")
    knn.analysis(haplogroup.data.frame, c(5, 6, 7), 8)

  }else if(args[2]=="3"){

    cat("Computing correlation indicators for U.k. haplogroup:", "\n")
    correlation.estimators(u, ftnd)
    cat("--------------", "\n")
    anova.analysis(h, j, u, ftnd)
    cat("--------------", "\n")
    cat("Computing logistic regression for U.k. haplogroup:", "\n")
    log.regression <- logistic.regression(u, ftnd)
    logistic.graph(u, ftnd, log.regression)
    cat("--------------", "\n")
    knn.analysis(haplogroup.data.frame, c(5, 6, 7), 8)

  }else{
    stop("The second argument for this script can only have a value of 1, 2, or 3.")
  }
