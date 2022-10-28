#Creating Utilities file

#Creating data.frame function
#same function used in other scripts
#need this to put csv file into a dataframe because this is what R works with
#always using na.omit to make sure the dataframe I have created is complete with all data to be able to run analyses
#returns the data frame
#note using variable names that are more general so this is more modular code and can be applied to any dataset I have

  create.data.frame <- function(csv.file){

    new.data <- read.csv(csv.file, header=TRUE, sep=",")

    new.data.frame <- data.frame(new.data)

    new.data.frame.clean <- na.omit(new.data.frame)

    return(new.data.frame.clean)

  }

#Create next function, for 2D argument
#this function is recieving the columns of interest, the xlim and ylim for the graph to ensure the data is plotted fully in the graph
#also recieves the xlabel and ylabel so that this function is more modular and can be used for any dataset
#also takes filename so that i can customize this for each plot i make depending on data

  plot.2D.func <- function(x, y, file.name, xlim, ylim, xlab, ylab, pdf.mar.wid=3.375, pdf.mar.height=2){

#data columns of interest passed to function
#create linear model for this graphing function
#also want to explore the quadratic fit visually on the graph
#although I used cor.test on both of these functions and I know linear is a better fit
#I still want to have this function visualize both models

    myfit <- lm(y ~ x)
    x2 <- x^2
    myquadfit <- lm(y ~ x+x2)

# 'pointsize' being used to set the axis label
#note this here will save as pdf which is correct way to save for paper
#not passing the margin size to the pdf function here because this is the standard size
#for publication so if needed I can go back and change but normally this is the correct size

    pdf(file = file.name, pdf.mar.wid, pdf.mar.height, pointsize = 6)

#creting specialized margins so that there is not a lot of white space around the graph
#again these are hardcoded because they are the size that creates the least white space with the publishable graph
    par(mai = c(0.28, 0.28, 0.03, 0.03))

#create the plot
#here turning off axes and ann so that I can customize those afterwards
#also passing xlim and ylim to function because those change depending on data so make the function more modular for different data
#Pch is the type of point and I have selected open circles for better visualization, as well as a size of 1.5 with cex
    plot(x, y, ann = F, axes = F, xlim = xlim, pch = 1, ylim= ylim, cex = 1.2)

#plot fit
#using a red line to have that pop out on the graph
#also plotting my quadratic function, using xx and yy and then lines()
#also making this one blue and smaller line width to be able to easily visualize this with linear and overlap
   abline(myfit, lty = 2, lwd=2.5, col="darkred")
   xx <- seq(min(x),max(x), len=length(x))
   yy <- myquadfit$coef %*% rbind(1, xx, xx^2)
   lines(xx, yy, lwd=1.2, col="blue2", lty=1)

#Adding a legend so that the reader knows what each line means
   legend(x="topleft", legend=c("Linear Model", "Quadratic Model"), col=c("darkred", "blue2"), lty=2:1, cex=0.9)
#add box around
   box()

#Adjusting the tick and axis label
#ticks are being centered on the x axis
#doing the position using "line" - the position of the xlabel and what is is called

    axis(side = 1, labels=NA)
    axis(side = 1, lwd=0, line= -0.3)
    title(line = 1.8, cex.lab = 1.1, xlab = xlab)

#same thing here for the y label customizing and making sure the ticks are nice and centered with line and label looks good on graph
#then using dev.off to make sure it closes and saves the pdf

    axis(side = 2, labels=NA)
    axis(side = 2, lwd = 0, line = -0.4)
    title(line = 1.8, cex.lab = 1.1, ylab = ylab)
    dev.off()

}
############################################################
#Notes on 2D graph:
#Here I am choosing to plot my independent variable which is the DTP (diptheria, pertussis, and tetnaus) vaccine coverage
#in % for each school in Toronto. This is being plotted against the MMR (measels, mumps, rubella) vaccine coverage in Toronto
#this data is from 2018-19 and is interetesing because it takes into accound elementary and middle schools so it is interesting to see
#how coverage rates relate to eachother considering new anti-vax movement
#I am plotting the data, and then using both a linear and quadratic fit overlayed on the same graph to show how these two models fit the data
#the linear model is statistically significant and a better fit numerically but it is interesting to see how the two compare especially for outliers where
#there is low coverage of DTP vaccine
#I am also using very specific margins and selecting all qualities fo the graph to ensure a nice publishable figure

############################################################
#Create 3D function
#this function will take the columns multivariate I am interested in
#also takes a vector of margin sizes I think are suitable for min white space
#lastly takes labesl for all axis, important so that this is modular and I can use for any data

  plot.3D.func <- function(x, y, z, file.name, mar.vec, xlab, ylab, zlab, pdf.mar.wid=3.375, pdf.mar.height=2){

#call the scatterplot3d library before
    library(scatterplot3d)

#next need to save this plot as a pdf and for this plot I am using a larger margin size so that graph shows up nicer as well as bigger pointsize so that it
#is a more ledgible graph
    pdf(file = file.name, pdf.mar.wid, pdf.mar.height, pointsize = 12)

#next using the scatterplot3d function and assigning this to new.scatter.3d
#note here I am making a lot of customization to the plot based on the different arugments it can take
#I am using type h to make a line up to y axis value, defining those lines as solid, using highlight.3d to have the different colours seen in
#graph like red to show where the data lies in respect to the y axis
#also scaling the angle and y axis for user optimized visability
#choosing the point type, box around, labels, and size of labels and symbols
#important here is margins are defined by a variable passed to function to have minimal whitespace
    new.scatter.3d <- scatterplot3d(x, z, y, mar=mar.vec, type="h", lty.hplot="solid", highlight.3d=T, angle=55,
    scale.y=0.5, box=T, pch=20, xlab=xlab, ylab=ylab, zlab=zlab, cex.symbols=0.7, cex.lab=1)


#now create the linear model where y is the dependent variable and x and z are independent and are on the correct axis
    new.lm.3d <- lm(y ~ x+z)

#next add the new linear model as a plane to the graph, using dotted and dark green colour because it is complimentary colour to red - better for visualization
#adding in the solid outside box on linear model plane as well as a thicker line width to show up better
    new.scatter.3d$plane3d(new.lm.3d, lty="dotted", lty.box="solid", col="darkgreen", lwd=1.5)

#always use dev.off() to close the grah and save in pdf
    dev.off()

  }

############################################################
#Notes on 3D graph:
#here I am using a 3D scatterplot to explore my x and y variables with my third explanatory variable on the z axis
#now I am including enrolled population so the number of students in each school in toronto that was analyzed for this coverage data
#this is interesting because it may help explain why some schools have a lower coverage rate, smaller school population meaning few kids
#not vaccinated would skew the results
#Here I also add a plane of this new linear model which adds z as an explanatory variable
#it very nicely lies over the data and it is easy to see this linear model fits this mulitvariate data analysis well
#Using this graph because it is a good way to visualize three variables and the associated linear model
