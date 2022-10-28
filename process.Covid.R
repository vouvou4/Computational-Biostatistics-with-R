#My process.Covid.R Driver script

#First import the functions to my driver script using source
  source("Covid.Utilities.R")


##################################################
#Creating code line to read covid.file as my command line argument from bash terminal
#Note, in driver script we write code as though we are at the terminal in R inputting commands
#First need to identify that covid.file is a command line argument using commandArgs()
#Next implementing 3 defensive coding lines to ensure the correct number and file is given
#using length to make sure no more than 1 file is given, if there is stop
#using length to make sure a file is given, will stop with no filename in the command line argument
#using file.exists() to say if a file name at command line does not exist in the directory, then stop running
#the covid.data variable represents the data frame that will be passed along to all other functions in Utilities file


  covid.file <- commandArgs(trailingOnly = TRUE)

  if(length(covid.file) > 1) {
    stop("Too many files presented. Choose one file only")
  }

  if(length(covid.file)==0){
    stop("We require a filename to process as a command line argument")
  }

  if (!file.exists(covid.file[1])) {
    stop("This is not a valid filename in the directory. Please enter a valid filename")
  }

  covid.data <- select.covid.file(covid.file)


##################################################
#Reporting patients.per.source.infection() to output screen based on file from command line
#using cat here to have our string presented to screen before the patients.per.source.infection() data is presented

  cat("Total number of patients per infection source:", "\n")
  patients.per.source.infection(covid.data)

##################################################
#Reporting number of cases in 40 to 49 years age Group confirmed to output screen from covid.data data frame
#Using the age.confirmed.cases() which is defined in the utilities file

  age.confirmed.cases(covid.data)

##################################################
#Reporting neighbourhood with most fatalities to output screen from covid.data data frame
#Using the neighbourhood.fatalities.max() which is defined in the utilities file

  neighbourhood.fatalities.max(covid.data)
