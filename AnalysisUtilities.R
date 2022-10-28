#Creating Utilities file for assignment 8

#First create function to read csv file of interest
#use read.csv to read the file, note this file is separated by ,
#then put this into a data frame which is what R works with
#note, using na.omit to ensure all NA from data is removed even if I have previously cleaned the data, this is a good check
#plink so there is no missing data
#finally return the data frame, this will be used in analysis

  create.data.frame <- function(csv.file){

    haplogroup.data <- read.csv(csv.file, header=TRUE, sep=",")

    haplogroup.data.frame <- data.frame(haplogroup.data)

    haplogroup.data.frame.clean <- na.omit(haplogroup.data.frame)

    return(haplogroup.data.frame.clean)

  }

#Create function to first compute the correlation estimators for the data
#Note, in this data we are interested in looking at three different haplogroups and if they are associated
#with an individuals FTND score (score for severity of nicotine addiction)
#but these functions can only take an x and y so at level of main driver I will need to run it three times
#for each explanatory variable
#Here just using the built in R functions to compute the correlation indicators to get a feel for the dataset
#not returning anything just printing to the screen because will not use any of these in later analyses


  correlation.estimators <- function(x, y){

    haplo.covariance <- cov(x, y)

    print(c("Covariance:", haplo.covariance))

    haplo.correlation <- cor(x, y)

    print(c("Correlation coefficient:", haplo.correlation))

    haplo.cor.test <- cor.test(x, y)

    print("Correlation Test:")

    print(haplo.cor.test)

}


#Next functions will go into hypothesis testing which is what is needed for this dataset
#we are interested in running these hypothesis tests to determine whether our alternative hypothesis is TRUE
#meaning the haplogroups are significantly associated with the FTND score
#using the Anova, this is the correct test for 1 IV with 2 or more levels(haplogroup), and an interval dependent variable
#that is why I choose to use a one-way ANOVA
#to do this I slice my FTND column by each haplogroup to get those individuals FTND Score
#then I use c() to combine and also combine categories aka the haplogroup names
#also using length of the variable which will give value of all the people in each group - better than hard coding
#next I combine this into a new data.frame
#Finally I can plot the haplogorups boxplots to see the average ftnd score
#from there we compute the actual anova to see numerically if it is significant
#print and return results

  anova.analysis <- function(x, z, b, y){

      cat("Computing ANOVA for all haplogroups:", "\n")

      h.anova <- y[x == 1]
      j.anova <- y[z == 1]
      u.anova <- y[b == 1]

      ftnd.anova <- c(h.anova, j.anova, u.anova)
      haplogroup <- c(rep("H.HV.V", length(h.anova)), rep("J.T", length(j.anova)), rep("U.k.", length(u.anova)))
      anova.df <- data.frame(ftnd.anova, haplogroup)

      par(mfrow = c(2,2))
      boxplot(ftnd.anova ~ haplogroup, data=anova.df, ylab = "FTND Score")

      results <- aov(ftnd.anova ~ haplogroup, data=anova.df)

      print(summary(results))
      return(results)

    }

#Compute logistic regression next for this dataset
#note here using glm with the binomial family because of binomial dataset for haplogroups
#because of the nature of the dataset I need to do x ~ y so that the dependent variable is binomial
#to be able to compute this logistic regression

  logistic.regression <- function(x, y){

    log.regression <- glm(x ~ y, family = binomial)

    print(summary(log.regression))

    return(log.regression)

  }

#Graph the logistic regression Analysis
#note in graph only see values on 0 and 1 because it is a binomial dataset for the haplogroup which is
#why I computed a logistic regression
#also note that because the haplogroup is binomial, I need to plot y and then x based on the glm above
    logistic.graph <- function(x, y, log.regression){

      plot(y, x, xlab ="FTND score", ylab="Haplogroup", main="Logistic Regression")

      abline(log.regression)

    }

#Create last function to run knn for this dataset
#this fucntion takes the data.frame of interest, the col.predict which is the columns
#I will use to predict the dependent variable with (note this is given as c(...))
#and col.target is the target column that I want to predict my values based on the haplogroups


    knn.analysis <- function(data.frame, col.predict, col.target){

      cat("Running kNN Algorithm for all haplogroups:", "\n")

#First generate a random number from the rows of the data frame, and telling it to do this for 90% of the rows in dataset

        ran <- sample(1:nrow(data.frame), 0.9*nrow(data.frame))

#Creating function to normalize the data columns of interest
        nor <- function(x){
          (x-min(x))/(max(x)-min(x))
        }
#use lapply function to apply to all rows of data frame, the function we just created which will normalize the data
#note telling it to return it as a data frame
#also note the col.predict is passed to function and these are the haplogroup columns
        haplo.norm <- as.data.frame(lapply(data.frame[, col.predict], nor))

#here extracting the training and testing set needed for knn analysis
#remember this alogorithm takes the data you give it and from there aims to predict
        haplo.train <- haplo.norm[ran,]
        haplo.test <- haplo.norm[-ran,]

#here extracting the column of interest, the FTND_Score column which is the one that will be predicted
        haplo.target.cat <- data.frame[ran, col.target]
        haplo.test.cat <- data.frame[-ran, col.target]
#call library(class) because it is needed to run the knn
        library(class)
#finally can use knn function which looks at each row of test set for the 'k' nearest neighbours in the training set
#this is how the model predicts the target column
#then put in table the prediction and the test data frame of interest
        predict <- knn(haplo.train, haplo.test, cl=haplo.target.cat, k=13)
        predict.table <- table(predict, haplo.test.cat)


#finally create function to determine the accuracy of this model for determining FTND score
        accuracy <- function(x){
          sum(diag(x)/(sum(rowSums(x))))*100
        }


#print to screen what the accuracy of the model is for the data frame and columns of interest
        cat("The accuracy of the model is:", "\n")
        accuracy(predict.table)

    }

#note, this knn predictive function could be used for any dataset and columns of interest!
