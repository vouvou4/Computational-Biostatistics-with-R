#creating new testingFns.R file


#First creating the test.quadratic.determinant functions
#This function will take all the quadratic equation coefficients as arguments
#Will also take det.value which represents the known determinant value which will test against
#Lastly gets test.name which needs to be passed as a string to give the output of which test you are running
#comparing the absolute value to a small value because this is the correct way to compare if two numbers are equal in a computer
#note that here you are making sure that your quadratic.determinant function works and computes the right output
#when correct output is computed it will be less than 1e-10 (0) and function will pass test
#Note, the ==> PASSED is just stylistic and shows up better visually at terminal

test.quadratic.determinant <- function(a, b, c, det.value, test.name) {

    if(abs(quadratic.determinant(a, b, c) - det.value) < 1e-10){
      cat("==> PASSED", test.name, "test.\n")
    }else {
      stop("Failed", test.name, "test.\n")
  }
}
###############################################################
#creating test.quadratic.roots function
#Here it takes the arguments of a, b, and c because need those to be passed to quadratic.roots functions
#Assigning the quadratic.roots() to quad.root variable for simplicity and so it is not redundant
#also include root.value.pos and root.value.neg which will be my known root values to test against
#next including test.name to cat that at the end so I can specify which test in the driver script
#the if statements are testing the quadratic.roots to make sure that the function correctly calculates both roots compared to known values
#Note that [[]] is used to index where in my list from that function I want to compare
#If I don't use [[]] then it is giving a list to a numeric operator which does not work
#Note, using 1e-5 because when subtract the function value from true root value it is of decimal place 1e-6


test.quadratic.roots <- function(a, b, c, root.value.pos, root.value.neg, test.name){

    quad.root <- quadratic.roots(a, b, c)

    if(abs(quad.root[[1]] - root.value.pos) < 1e-10){
      cat("==> PASSED first", test.name, "test.\n")
    }else {
      stop("Failed", test.name, "test.")
    }

    if(abs(quad.root[[2]] - root.value.neg) < 1e-10) {
      cat("==> PASSED second", test.name, "test.\n")
    }else {
      stop("Failed", test.name, "test.")
    }

}

###############################################################
#creating test.quadratic.eqn
#giving all the required arguments, in this case will test determinant, first.deriv[[1]], and second.deriv
#assigning quadratic.eqn() to variable quad.list for simplicity of code and readability
#creating three separate if statements for each section I am testing of the code
#Again using the abs() < 1e-10 to make sure that the value computed is equal to the known value
#the output should show each specific test is passed based on values passed from driver script
#using $ here to select specific parts of the list in quadratic.eqn()

test.quadratic.eqn <- function(a, b, c, det.value, first.deriv.1, second.deriv, test.name){

    quad.list <- quadratic.eqn(a, b, c)

    if(abs(quad.list$determinant - det.value) < 1e-10){
      cat("==> PASSED", test.name, "determinant test.\n")
    }else {
      stop("Failed", test.name, "determinant test")
    }

    if(abs(quad.list$first.deriv[[1]] - first.deriv.1) < 1e-10){
      cat("==> PASSED", test.name, "first derivative #1 test.\n")
    }else {
      stop("Failed", test.name, "first derivative #1 test")
    }

    if(abs(quad.list$second.deriv - second.deriv) < 1e-10){
      cat("==> PASSED", test.name, "second derivative test.\n")
    }else {
      stop("Failed", test.name, "second derivative test")
    }

}

###############################################################
#creating test.petit.pain.au.lait
#Using same approach as quadratic.eqn but instead here I am subtracting two vectors
#will subtract the Amounts column from the known amounts vector I will give as an argument (c(...))
#again assigning the petit.pains.au.lait() to a variable here
#Had to add the all in the bracket of if statment so that if can make sure each value in Amount column is equal to known value
#If you don't have all then it will only compare the first TRUE boolean vector, we need to make sure it checks all are TRUE to pass test


  test.petit.pain.au.lait <- function(quant= 12, petit.known.vec, test.name){

    petit.recipe <- petit.pains.au.lait(quant)

    if(all(abs(petit.recipe$Amounts - petit.known.vec) < 1e-10)){
      cat("==> PASSED", test.name, "test.\n")
    }else {
      stop("Failed", test.name, "test")
    }

  }
