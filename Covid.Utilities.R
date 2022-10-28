#Making Utilities file which will contain multiple functions outlined below
#These functions will be called by my R driver script to run the appropriate functions on the data

###################################################################
#Creating select.covid.file function
#reads .csv file from argument input, the argument file name comes from driver script selection, file must be put in with ""
#note that the .csv file argument is coming from the command line argument in Linux shell
#puts data into data frame, and prints name of file being processed
#finally it return the data

select.covid.file <- function(covid.file){
    covid.data <- read.csv(covid.file)
    covid.data.frame <- data.frame(covid.data)
    cat("Processing data from file:", covid.file, "\n")
    return(covid.data.frame)

}

###################################################################
#Creating patients.per.source.infection function
#Function identifies sources of infection using unique() - this gives independent levels
#Function loops over using identified sources and then adds total for each source
#source.infection.column represents the column that lists source infection
#variables.of.infection represents the different sources of infection
#column is Source.of.Infection in data
#indiv.source.data is slicing the column w/ each source, total.patients.per.source is sum of all the rows that are sliced
#finally use cat to print out the sources of infection as well as the total number for each source

patients.per.source.infection <- function(covid.data) {
    source.infection.column <- covid.data$Source.of.Infection
    variables.of.infection <- unique(source.infection.column, incomparables = FALSE)
    for(i in variables.of.infection) {
      indiv.source.data <- source.infection.column[source.infection.column==i]
      total.patients.per.source <- sum(indiv.source.data == i)
      cat(i, "--", total.patients.per.source, "\n" )
      }
}
###################################################################
#Creating age.confirmed.cases
#column is Age.Group
#age.column.cases is variable defining my search in the column for the age group of interest
#using slicing to slice out the Age.Group and Classification column "CONFIRMED"
#need to print the number of rows for this specific sliced data
#total.num represents the summed up rows in the age column of people who were "40 to 49 Years" & "Confrimed"
#finally cat out the result in a sentence

age.confirmed.cases <- function(covid.data) {
    age.column.cases <- covid.data$Age.Group[covid.data$Age.Group=="40 to 49 Years" & covid.data$Classification=="CONFIRMED"]
    total.num <- sum(age.column.cases == "40 to 49 Years")
    cat("The number of confirmed cases in the 40-49 years age group is", total.num, "\n" )
    }

###################################################################
#Creating function that calculates neighbourhood with most fatalities
#Prints the neighbourhood with the max amount of fatalities
#Neighbourhood.Name is column title, slicing here to filter column with "FATAL" from outcome column
#neighbor.name is the variable representing neighbourhood with max fatalities
#max.value is variable representing the actual max number of fatalities there
#finally printing out the number of fatalities and neighborhood in sentence with cat

neighbourhood.fatalities.max <- function(covid.data){
    neighbourhood.fatalities.column <- covid.data$Neighbourhood.Name[covid.data$Outcome=="FATAL"]
    table.analysis <- table(neighbourhood.fatalities.column)
    neighbor.name <- names(table.analysis[which.max(table.analysis)])
    max.fatal.cases.value <- max(table.analysis)
    cat("The neighbourhood with the most fatalities is", neighbor.name, "with", max.fatal.cases.value, "fatalities", "\n")
}
