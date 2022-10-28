#Creating FittingUtilities.R file

#Create first func to load data and create data.frame, this function takes the concussion file as an argument
#first use read.csv() to read file. Note that this file is separated by TABS so use sep="\t"
#Next put this into a data.frame for further analysis
#Next use na.omit() to remove any NA rows from the dataset I am analyzing and then assign that to my data.frame.clean
#Lastly return this concussion.data.frame.clean
#From here this whole function will be assigned to my data.frame.clean name at the level of the main driver script and then passed down to my remaining functions
#Note, when you assign a function which returns something to a variable that variable then has the value of just the returned output of that function


  create.concussion.data.frame <- function(concussion.file){

    concussion.data <- read.csv(concussion.file, header=TRUE, sep="\t")

    concussion.data.frame <- data.frame(concussion.data)

    concussion.data.frame.clean <- na.omit(concussion.data.frame)

    return(concussion.data.frame.clean)

  }

#Creating second function that will compute the correlation estimators of the data
#Will pass only x and y to function which is defined at level of main driver script
#x and y are used to match output and they represent the data.frame[,col.1] and data.frame[,col.2] respectively
#In this analysis I am using columns Nbr.of.TBIs, and Nbr.of.concussions
#Then compute cov, cor, and cor.test using these functions, and print them out to the screen
#Note: cor.test cannot be printed with c as it ruins the output so instead it needs to be printed on its own
#Observations for this data: very large value of covariance due to the large variation of the data values (ranging from 200 range to 10s)
#correlation coefficient of 0.999 meaning a very strong positive correlation between the number of TBIs and number of concussions in each sport
#also cor.test shows a very small p-value and a very tight 95% confidence interval meaning the true value of the correlation coefficient lies between 0.998 and 0.999
#so overall from these I can tell there is a very strong positive correlation between these two variables

  comp.corr.estimators <- function(x, y){

        cat("Computing correlation indicators...", "\n")

        conc.covariance <- cov(x, y)

        print(c("Covariance:", conc.covariance))

        conc.correlation <- cor(x, y)

        print(c("Correlation coefficient:", conc.correlation))

        conc.cor.test <- cor.test(x, y)

        print("Correlation Test:")

        print(conc.cor.test)
}

#Create function to implement the linear model to fit the data and print out the details of fitted models
#This function similarly recieves x and y - better for simplicity and no repeated code in comparison to assigning x and y in each function
#here create linear model using lm, and then print the summary of this model as well as return that model to be used for creating a graph
#In the linear model I notice the coefficient estimate is close to 1 (0.95) for the linear portion of the model and the p value is ver significant
#From this I can tell that the data will be fitted quite well to a linear model as the correlation coefficient is so large meaning it will contribute greatly to
#the predicted values of y
#Ultimatley the data points should fall closely to the fitted model line because of the strong correlation coefficient and therefore the strong contribution the linear portion of the model is providing
#Note you need to graph the data to be sure about the strong correlation it is not enough to look at the correlation coefficient


    comp.linear.model <- function(x, y){

      cat("Fitting a Linear Model", "\n")

      conc.linear.model <- lm(y ~ x)

      print(summary(conc.linear.model))

      return(conc.linear.model)

    }


#Create function to make a graphical representation of linear model
#This function plots the data we are interested in (the appropriate columns of the data.frame)
#Next it adds the line to the graph and this line is representing that linear model I just created above
#note the linear model above is assigned to the value of linear.model at the level of the main driver script
#note: this function takes an additional argument of linear.model which is needed to create the line on the graph
#Note: when assigning arguments default values put those at the end or else they will not run correctly
#When inspecting this graph it is evident that the linear model that was created to fit this data is indeed a very good fit
#The line representing the linear model intersects almost all data points in this graph meaning it models the relationship between these two variables well
#From this I can interpret that the number of TBIs for a sport is strongly correlated with the number of concussions observed for that sport

  linear.graph <- function(x, y, linear.model){

    plot(x, y)

    abline(linear.model)

  }

#Create a function that will fit a quadratic model for the data
#This function takes similar arguments as above and also uses x and y for simplicity and to match assignment output
#Here first need to square the x column to create the quadratic model
#Next use the linear model function again but instead of just doing y ~ x now need to do y ~x+x.squared as this is the definition of a quadratic model
#The x.squared portion is providing the quadratic contribution to the linear model
#Then print the summary and return the model because again this will be assigned to a quad.model at the level of main driver script
#My observations here are that when looking at the coefficient values for x the linear contribution it is almost the same as above coefficient (0.97)
#Interestingly, x.squared representing the quadratic contribution's coefficient value is extremely small (e-05) therefore this quadratic component Will
#contribute very little to the quadratic model, instead the model will mainly be dictated by the linear contribution
#this is because this quadratic model is defined by (A x x.squared) + (B x x) + y so the first term defining the quadratic component will contribute very little
#to the model and the model will be defined mainly by the second term and y which is the definition of a linear model - therefore linear model fits better for this data
#also note that df is 26 for the quadratic model instead of 27 for linear - this is because of the definition of quadratic that squared term takes out one extra degree of freedom

  comp.quad.model <- function(x, y){

    cat("Fitting a Quadratic Model", "\n")

    x.squared <- x^2

    conc.quad.model <- lm(y ~ x+x.squared)

    print(summary(conc.quad.model))

    return(conc.quad.model)

  }


#Create function to graph the quadratic model
#This function takes similar arguments but also takes quad.model which is defined at the level of the main driver script
#To plot the quadratic model first need to plot the data and then create the xx and yy values
#xx values are created through seq() and yy are predicted points of the model in the range of y values
#and so take the quad.model coefficeint value and use matrix multiplication of the xx value and xx^2(quadratic) values
#finally use lines to add this line to the graph
#When looking at this graph it looks almost identical to the linear graph created above
#This is because of what I described above, the quadratic component is so small it contributes almost nothing to the model
#and so the model is being determined by the linear contribution
#this indicates that the linear model is the best fit for this data as it accounts for almost all of the data points
#and accuratley captures the strong positive linear correlation between the number of TBIs and number of concussions



  quad.graph <- function(x, y, quad.model){


    plot(x, y)

    xx <- seq(min(x),
              max(x),
              len=length(x))

    yy <- quad.model$coef %*% rbind(1, xx, xx^2)

    lines(xx,yy, lwd=2, col=2)

  }



#Adding new function for assignment 7 named AnalyzeModel()
#The purpose of this function is to generate seveal model diagnostic plots and mark the suspicious points in these plots based on tolerance level
#tolerance level (tol.level) has a default value of 0.25
#first create cooks graph from here we can then identify our influential points
#plot cooks graph, then determine the threshold points using slicing with tol.level
#next use as.integer() to get the integer value of rows that were greater than tol.level
#finally use points() to make those points red
#Use similar method as above for creating and marking suspicious points in the leverage plot
#Note cooks distnace is used because it is the points that will be used for all graphs
#this is because cooks distance is more robust it looks at both outliers and leverage
#Next create residuals plot (3 panels)
#plot in each of the panels the residual points of model
#Use points again to mark in red the cooks distance points previously identified
#Create studentized residuals plot after which is again similar code to residuals plot
#the studentized residual actually represents the quotient from diving a residual by an estimate of its SD
#then create qq plot as well as histogram
#these are both plotting the residuals to look at normal probability and ensure the normality assumption of data is true



  AnalyzeModel <- function(concussion.data.frame.clean, x, y, model, tol.level=0.25){

  #Create cooks distance plot

      cookdist <- cooks.distance(model)

      plot(cookdist, ylab="Cook's Distance")

      cook.thresh.points <- concussion.data.frame.clean[cookdist>tol.level,]

      final.cook.points <- as.integer(rownames(cook.thresh.points))

      points(final.cook.points, cookdist[final.cook.points], col="red")

      #Create leverage plot

      lev <- hat(model.matrix(model))

      plot(lev)

      points(final.cook.points, lev[final.cook.points], col="red")

  #Create residuals plot
  #Note must use points() after every plot as it corresponds to the plot that was just made

      par(mfrow=c(1,3))
      plot(x, model$res)
      points(x[final.cook.points], model$res[final.cook.points], col="red" )
      plot(y, model$res)
      points(y[final.cook.points], model$res[final.cook.points], col="red" )
      plot(model$fitted, model$res)
      points(model$fitted[final.cook.points], model$res[final.cook.points], col="red" )

  #Create studentized residuals plot

      model.rs = rstudent(model)
      par(mfrow=c(1,3))
      plot(x, model.rs)
      points(x[final.cook.points], model.rs[final.cook.points], col="red")
      plot(y, model.rs)
      points(y[final.cook.points], model.rs[final.cook.points], col="red")
      plot(model$fitted, model.rs)
      points(model$fitted[final.cook.points], model.rs[final.cook.points], col="red")

  #Create qq plot
  #this plot as well as hist does not need the suspicious points noted in red

      qqnorm(model$res)
      qqline(model$res)

  #Create histogram plot

      hist(model$res)

  #print suspicious points and return a list of these points according to tolerance level
  #note that for return if use just list() it will put all three integers into [[1]] of list
  #we don't want this instead want to have a list returned with each point in own [[]] so use as.list

    print("Suspicious points:")
    print(final.cook.points)

    return(as.vector(final.cook.points))
  }
###################################
#Observations about the residuals and suspicious points for linear and quadratic
#Here we are using very strict tolerance level of 15%, normally it is 5%
#the linear model has three suspicious points that get removed and the quadratic has only two
#This indicates that the linear has more points that are either outliers or larger leverage (definition of cooks distance)
#these points are residuals that may affect the true signifcance and fit of the model which is why at the level of the driver they are removed
#when you run the plots again for linear and quadratic after removing suspicious points you see an even better fit of the line(model) with the data
#note residuals are the statistical estimation of the difference between model and actual dataset
#so if we say linear model fits the data, well how much true difference is there between the linear model and the data we have
#note also if there are three suspicious points in linear and only 2 in quadratic this means that there are more points that when removed from data will significantly change the fit to the models

###################################

#Creating new function to implement a generalized model for columns Nbr.of.TBIs, Nbr.of.concussions, and Nbr.of.other.injuries
#note, here we need to use two explanatory variables because we will be plotting it as a 3D model
#using the glm formula here to create the generalized model, note if no family of error distribution is defined the default is gaussian
#Finally returning the summary of the new model


  generalized.model <- function(x, y, z){

    cat("Fitting a Generalized Linear Model", "\n")

    glm.concussion <- glm(y ~ x+z, family=Gamma)

    print(summary(glm.concussion))

    return(glm.concussion)

  }
#######################
#Create function to graph the glm
#create the scatter3d plot using x, z, and y
#note in code need to include library(scatterplot3d) or else it will not run because it is not an innate function of r
#note: z is our new variable which is defined at the level of the driver script and then passed into the function
#next slicing the 3dplot by plane3d to insert a plane to this 3d plot and the plane is the new glm I have created


  generalized.model.graph <- function(x, y, z, generalized.model){

    library("scatterplot3d")

    scatter3d <- scatterplot3d(x, z, y)

    scatter3d$plane3d(generalized.model)

  }

####################################

#Observations for generalized model:
#purpose of this model is to extend linear modeling for scenarios with non-normal error distributions and heteroscedasticity
#Here I am using the Gamma family of errors for my generalized linear model because after
#inspecting the quantatative results of all possible families it is clear that the Gamma is the best fit
#note the binomial  would not be approporiate for the predicted variable which in this case is number of concussions
#as binomial looks at binary data
#when the gaussian family was used the plane looked just like a linear model and so it does not correct for a non-normal error distribution which is what this model is trying to accomplish
#the poisson (for discrete events) fit better but not as good as the Gamma
#I know the Gamma was the best because the residual deviance as well as the null deviance was the lowest for this family meaning that this model is the best fit
#therefore based on quantative and qualatative analysis I have determined that the Gamma family of errors is the best fit for this generalized linear model
#note: for Gamma there is a higher AIC than guassian, but for Gamma both explanatory variables are significant whereas in gaussian only x is
#additionally, the 3d plot for gaussian looks better than Gamma as the plane goes through the points but only for x and y
#Overall I think that this generalized model probably is not the best fit for this data and an lm is sufficient but for the purpose of this excercise I would go with Gamma because
#of signficance and lower residual deviance
