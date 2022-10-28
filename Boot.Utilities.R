#create utilities file

# use same fn created before to read csv file and make it into a dataframe
#note, alwasy omiting NA from csv files so that it does not hamper the rest of the analyses
#here also adding sep as an argument because different csv files may have different separators

  create.data.frame <- function(csv.file, sep){

    new.data <- read.csv(csv.file, header=TRUE, sep=sep)
    new.data.frame <- data.frame(new.data)
    new.data.frame.clean <- na.omit(new.data.frame)
    return(new.data.frame.clean)

  }


#########################################################
#Here I am making the bootstrap function and I'm nesting another function within this one
#first selecting my user data I am interested in for the user rating kids Movie - do this using slicing of the dataframe
#next simply using the library boot for the boot function
#before that creating a function to determine the statistic of interest which for this assignment is the mean
#then call the boot function and give all the correct arguments like the data column of interest, the statistic to be used which is
#the previously defined nested functino as well as the iterations to be completed
#finally it returns the bootstrap t value which is the list of iterations and the mean for each iteration
#note this function takes the data.frame, as well as the columns of interest and values for those columns
#of interest as well as the number of iterations
#trying to make the function as modular as possible so nothing is hard coded and everything can be passed in

  bootstrap.analysis <- function(mov.data, col1, user, col2, col.value.bool, col3, iterations){

    user.data <- mov.data[mov.data[, col1]==user & mov.data[, col2]==col.value.bool, ]
    library(boot)

    my.mean <- function(data, i){
      return(mean(data[i]))
    }

    boot.strap <- boot(data = user.data[, col3], statistic = my.mean, R = iterations)
    return(boot.strap$t)

  }

#########################################################
#create function for the permutation test that needs to be run to compare both of the bootstrap analyses

#Here the function takes the two bootstrap data results for each person, the value for the alternative hypothesis, method to be used,
#number of permutation sample as well as tsmethod and significance value, all can be customized
  permutation.test <- function(user.boot.data1, user.boot.data2, alternative.hypo, method, nbr.of.perm.sample, tsmethod, sig.value){

#call library perm for permutation
    library(perm)

#using the permTS fn, passing all data in from function
#using permControl to control exactly the number of permutation samplings and how to calculate the two-sided p-vlaues
    perm.test <- permTS(as.vector(user.boot.data1), as.vector(user.boot.data2), alternative=alternative.hypo, method=method,
      permControl(nmc = nbr.of.perm.sample, tsmethod = tsmethod))

#Next using if statement chain to have function act accordingly based on the p.value
#if p-value is significant, less than sig.value, then tell screen they are statistically significant and the p-value
#calling p-value using $ because it is a column from output and easily accessed this way
    if(perm.test$p.value < sig.value){
      cat("The movie reviewers are statistically significantly different, with a p value of", perm.test$p.value, "\n")
#Here if not significant, greater than p-value, then print to screen it is NOT significant while printing p-value
    }else if(perm.test$p.value > sig.value){
      cat("The movie revieweres are NOT statistically significantly different, with a p value of", perm.test$p.value, "\n")
    }

  }

#########################################################
#Anova function
#this function takes the users to be used, the names for the columns of the dataframe to be created, and x and y labels for boxplot

anova.analysis <- function(user.1, user.2, user.3, user.1.name, user.2.name, user.3.name, xlab, ylab){

#Here creating a dataframe with the 3 users bootstrap iteration outputs that will be used for anova analysis
#passing all the arguments in, combining the data, and defining the columns as well as the length of the columns which corresponds to the correct values for
#those columns with data combined above
#lastly make the df, with appropriate rows and columns
    anova.data<- c(user.1, user.2, user.3)
    anova.col.names <- c(rep(user.1.name, length(user.1)), rep(user.2.name, length(user.2)), rep(user.3.name, length(user.3)))
    anova.df <- data.frame(anova.data, anova.col.names)

#Here adding in the creation of a boxplot because I think it is a nice step to have before running the statistical Anova
#gives a visual representation of the data to see if visually there are differences between the average ratings of the 3 users after x number of bootstrap iterations
    boxplot(anova.data ~ anova.col.names, data=anova.df, xlab = xlab, ylab = ylab)

#finally running the anova here
    results <- aov(anova.data ~ anova.col.names, data=anova.df)

#return results of the anova to be passed on to post hoc analysis
    return(results)

  }


#########################################################
#Function for posthoc test and report result of Anova
#this takes the anova results as well as significance and confidence level as arguments

  post.hoc.analysis <- function(anova.results, sig.value, conf.level){

#Here assigning the value of the summary of the anova results to a variable so I can slice out the p-value as it is not reported simply from the results output of anova
#this is needed for the if statements
      anova.summary <- summary(anova.results)

#Here I use slicing of that anova.summary defined above to index from the summary list the p-value which is then used to determine which chain of commands is used
#if the p-value is significant then run the TukeyHSD test
      if((anova.summary)[[1]][["Pr(>F)"]][[1]] < sig.value){

#Here running Tukey post hoc test, and can only use anova.results here not the summary, and also pass the confidence level of interest
#Then tell user they are significantly differet, the p-value, and the TukeyHSD results to determine which groups are significantly different
        tukey.test <- TukeyHSD(anova.results, conf.level=conf.level)
        cat("The movie revieweres are statistically significantly different, with a p value of", (anova.summary)[[1]][["Pr(>F)"]][[1]], "\n")
        cat("TukeyHSD results shown below:", "\n")
        print(tukey.test)

#if not significant then tell the user they are not significantly different and the p-value, no need for post-hoc analysis on results that are not
#significant 
      }else if((anova.summary)[[1]][["Pr(>F)"]][[1]] > sig.value){
        cat("The movie reviewers are NOT statistically significantly different, with a p value of", (anova.summary)[[1]][["Pr(>F)"]][[1]], "\n")
      }

  }
