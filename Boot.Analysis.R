#Create driver script

#source utilities file
  source("Boot.Utilities.R")

  #create command line args

    args <- commandArgs(trailingOnly = TRUE)

  #Add defenses for these command line args
  #Defending against too many, too little arguments, and invlaid files
      if(length(args) <= 1){
        stop("This script takes two arguments. A csv file name and either perm or anova.")
      }

      if(length(args) >2){
        stop("Too many arguments presented. This script only takes two arguments, a csv file and either perm or anova.")
      }

      if(!file.exists(args[1])){
        stop("This file does not exist. Please enter the correct filename: MovieLens.small.csv")
      }

  #start running the script
  #use cat to tell the user which file we are processing

    cat("Processing data from file:", args[1], "...", "\n")
    cat("--------------", "\n")

  #Create the dataframe that will be used for this script
  #keep very modular with args[1] as the argument, no hard coding!

    movie.data <- create.data.frame(args[1], ",")

#next using if statements to have code run correct outputs depending on the second argument
    if(args[2]=="perm"){

#Here running bootstrap analysis for the two users of interest, passing the dataframe, columns, user number, boolean value for kids column, and number of iterations
#Next running the permutation test which takes the two bootstrap analysis results, the type of alt hypothesis, the method, number of permutation samplings, way to calculate test, and sig.value
      user.548.boot <- bootstrap.analysis(movie.data, "userId", 548, "kids", TRUE, "rating", 1000)
      user.8619.boot <- bootstrap.analysis(movie.data, "userId", 8619, "kids", TRUE, "rating", 1000)
      permutation.test(user.548.boot, user.8619.boot, "two.sided", "exact.mc", 1000, "central", 0.05)

#Here creating chain for anova as second argument
    }else if(args[2]=="anova"){

#Again running the bootstrap analyses for the now three users of interest, the first two are the same as above, now adding a third
#note the functions used and arguments are exact same
#Then running the anova and assigning that to a variable to be passed to posthoc analysis
#Anova is taking the three users bootstrap outputs, the users names for the data frame, and the x and y labels for the boxplots I added
      user.548.boot <- bootstrap.analysis(movie.data, "userId", 548, "kids", TRUE, "rating", 1000)
      user.8619.boot <- bootstrap.analysis(movie.data, "userId", 8619, "kids", TRUE, "rating", 1000)
      user.17783.boot <- bootstrap.analysis(movie.data, "userId", 17783, "kids", TRUE, "rating", 1000)
      anova.results <- anova.analysis(user.548.boot, user.8619.boot, user.17783.boot, "User548", "User8619", "User17783", "User", "Rating")

  #Lastly running the posthoc analysis which takes the anova results, the sig value, and confidence interval to complete the appropriate test based
  #on the p-value and print the appropriate results to the screen
      post.hoc.analysis(anova.results, 0.05, 0.95)
    }else{

#if incorrect second argument then script will stop and tell user the values of the second arugment
      stop("The second argument for this script can only have a value of perm or anova.")
    }
