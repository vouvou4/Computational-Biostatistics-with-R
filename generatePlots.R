#creating main driver

#source utilities file
  source("plottingTools.R")

#create command line args

  args <- commandArgs(trailingOnly = TRUE)

#Add defenses for these command line args
    if(length(args) <= 1){
      stop("This script takes two arguments. A csv file name and either 2D or 3D.")
    }

    if(length(args) >2){
      stop("Too many arguments presented. This script only takes two arguments, a csv file and either 2D or 3D.")
    }

    if(!file.exists(args[1])){
      stop("This file does not exist. Please enter the correct filename: merged.haplogroup.file.csv")
    }

#start running the script
#use cat to tell the user which file we are processing

  cat("Processing", args[1], "...", "\n")
  cat("--------------", "\n")

#Use the utility file function to create the data frame
#note here we are being very modular by calling args[1] instead of hardcoding the csv file
#this is the cleaned data frame used to select the columns of interest
#Here I am looking at the % of students in 2018-19 who were immunized for DTP and its relation
#to the amount of students immunized for MMR in Toronto.
#This is interesting because I am curious to see if the rates have changed and if there is more spread
#due to widespread misinformation and fear of vaccines in recent years
#note cor.test produced a 0.7 correlation between the two variables

  vaccine.data <- create.data.frame(args[1])

  x <- vaccine.data[, "DTP.coverage.rate...."]
  y <- vaccine.data[, "MMR.coverage.rate...."]
  z <- vaccine.data[, "Enrolled.population"]

#Run 2D plot if 2D is at command line
#also using cat to let user know which plot you are making and for which file

  if(args[2]=="2D"){

    cat("Creating 2D plot for", args[1], "\n")

    plot.2D.func(x, y, "Toronto.Immunization.2D.plot.pdf", c(30, 100), c(50, 100), "DTP Vaccine Coverage Rate [%]", "MMR Vaccine Coverage Rate [%]")

  }else if(args[2]=="3D"){

  #making 3D plot for 3D argument, similarly using cat
  #also giving one more axis title to this function"

    cat("Creating 3D plot for", args[1], "\n")

    plot.3D.func(x, y, z, "Toronto.Immunization.3D.plot.pdf", c(3, 2.5, 0.01, 1.9), "DTP Vaccine Coverage Rate [%]", "Enrolled Population [Number of Students]", "MMR Vaccine Coverage Rate [%]",
        7.47, 5.5)

  }else{
    stop("The second argument for this script can only have a value of 2D or 3D.")
  }
  #last stop function here to protect script from running something that is not correct second command argument
