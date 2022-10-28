#Creating generateModels.R driver script

#source utilities file
  source("FittingUtilities.R")


#Create command lind arguments
  concussion.file <- commandArgs(trailingOnly = TRUE)


#Add defensive programming to command line arguments

#Add defense if no arguments are presented
  if(length(concussion.file)==0){
    stop("No arguments presented. This script requires two arguments: a CSV file and an option 1, 2, or 3.")
  }

#Defense against if the file does not exist for the first argument
    if(!file.exists(concussion.file[1])){
      stop("This file does not exist. Please enter the correct filename: concussion.csv")
    }

#Add defense if more than 2 arguments are presented
  if(length(concussion.file) >2){
    stop("Too many arguments presented. This script only takes two arguments: a CSV file and an option 1, 2, 3, 11, 12, 21, 22, or 30.")
  }

#Protect against no second argument presented
  if(length(concussion.file)==1){
    stop("This script requires a second argument, either 1, 2, 3, 11, 12, 21, 22, or 30.")
  }

#Start running script
#Use cat to tell user what file you are processing

  cat("Processing file", concussion.file[1], "...", "\n")

  cat("--------------", "\n")

#Assign the data frame function to a variable that gets passed to all the Utilities functions
#note this variable concussion.data.frame.clean only has the value of what is returned in the function which in this case is the data.frame

  concussion.data.frame.clean <- create.concussion.data.frame("concussion.csv")


#Here assigning the columns of interest to x and y at level of main driver script
#these are then passed to each function in the Utilities file
#Note the user must input the columns they are interested here to slice the data.frame
#this is hard-coded but is acceptable because it is at the level of the main driver script whch can have variables assigned like this because they then are passed to the functions
#Because those two columns are what I am interested in for this analysis, but if I wanted to analyze other columns it is very easy to add in the new column names
#Note, when entering these column names as arugments you need to use "" as they are strings
#probably a better approach to have simplicity in the Utility file functions and not have a lot of repeated code


  x <- concussion.data.frame.clean[, "Nbr.of.TBIs"]

  y <- concussion.data.frame.clean[, "Nbr.of.concussions"]



#Create if chain for each of the 3 possible arguments
#First if 1, needs to compute the correlation estimators, then compute linear model using the fn() which is assigned to the value of linear.model
#note since comp.linear.model() has a print statement inside even when assigned to a variable it will still print the summary which is what we want
#next create the linear graph
#If it is 2, then do same steps but instead cread the quadratic model and quadratic graph
#Lastly, #3 needs to do both, so again do correlation estimators, then do the linear and quadratic models, and finally both graphs
#At the end we are protecting against anything that is not 1, 2, or 3 for the second argument, if something else is given to the script that is not one of those three it should stop.
#This is the way to create a chain of if and if else statements to run from driver and at the end use else stop to defend against anything else


  if(concussion.file[2]=="1"){

    comp.corr.estimators(x, y)

    cat("--------------", "\n")

    linear.model <- comp.linear.model(x, y)

    linear.graph(x, y, linear.model)

  } else if(concussion.file[2]=="2"){

    comp.corr.estimators(x, y)

    cat("--------------", "\n")

    quad.model <- comp.quad.model(x, y)

    quad.graph(x, y, quad.model)

  }else if(concussion.file[2]=="3"){

    comp.corr.estimators(x, y)

    cat("--------------", "\n")

    linear.model <- comp.linear.model(x, y)

    cat("--------------", "\n")

    quad.model <- comp.quad.model(x, y)

    linear.graph(x, y, linear.model)

    quad.graph(x, y, quad.model)

#Adding else if statements for 11 and 12 which will run either linear or quad model for Analyze model()
#11 and 12 are new command-line options from bash terminal

  }else if(concussion.file[2]=="11"){

    linear.model <- comp.linear.model(x, y)

    cat("--------------", "\n")

    AnalyzeModel(concussion.data.frame.clean, x, y, linear.model, 0.15)

  }else if(concussion.file[2]=="12"){

    quad.model <- comp.quad.model(x, y)

    cat("--------------", "\n")

    AnalyzeModel(concussion.data.frame.clean, x, y, quad.model, 0.15)

  }else if(concussion.file[2]=="21"){

#Here now creating the next steps where we compute linear model and use this for the AnalyzeModel()
#from here we assign that returned vector list to suspic.points
#next we clean the dataframe from those suspicious rows
#finally assign new x and y based off of format above and use those to compute new linear model and graph
#note to compute the new linear model and graph must use the new dataset columns of interest
#also note here using the functions previously defined in utilities file for creating the models and graphs


    linear.model <- comp.linear.model(x, y)

    cat("--------------", "\n")

    suspic.points <- AnalyzeModel(concussion.data.frame.clean, x, y, linear.model, 0.15)

    cat("--------------", "\n")

    clean.concussion.data <- concussion.data.frame.clean[-suspic.points, ]

    x.clean <- clean.concussion.data[, "Nbr.of.TBIs"]
    y.clean <- clean.concussion.data[, "Nbr.of.concussions"]

    cat("Computing new cleaned linear model:", "\n")
    new.linear.model <- comp.linear.model(x.clean, y.clean)

    cat("--------------", "\n")

    linear.graph(x.clean, y.clean, new.linear.model)

  }else if(concussion.file[2]=="22"){

#Using a similar approach as above instead for this one we are computing the quadratic model and graph
#Note all are using 0.15 tolerance level for the AnalyzeModel function

    quad.model <- comp.quad.model(x, y)
    cat("--------------", "\n")
    suspic.points <- AnalyzeModel(concussion.data.frame.clean, x, y, quad.model, 0.15)
    cat("--------------", "\n")

    clean.concussion.data <- concussion.data.frame.clean[-suspic.points, ]

    x.clean <- clean.concussion.data[, "Nbr.of.TBIs"]
    y.clean <- clean.concussion.data[, "Nbr.of.concussions"]

    cat("Computing new cleaned quadratic model:", "\n")
    new.quad.model <- comp.quad.model(x.clean, y.clean)
    cat("--------------", "\n")

    quad.graph(x.clean, y.clean, new.quad.model)

  }else if(concussion.file[2]=="30"){

#for this option I need to first implement a model
#this model should fit the data more generally based on the residuals' distribution
#need to define our z variable to be passed into the function
#note, x and y are defined outside of the if chain because they are used in all functions and at level of driver script this is acceptable
#then call functions from Utilities file to compute the model and create the 3d graph and plane
    z <- concussion.data.frame.clean[, "Nbr.of.other.injuries"]

    general.model <- generalized.model(x, y, z)

    generalized.model.graph(x, y, z, general.model)

  }else{
    stop("The second argument for this script can only have a value of 1, 2, 3, 11, 12, 21, 22, or 30.")
  }
