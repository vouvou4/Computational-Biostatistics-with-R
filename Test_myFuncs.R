#Creating Test_myFuncs.R file
#This is my driver script that will source my two utilities files which have all of my functions

  source("myFuncs.R")
  source("testingFns.R")

#Next need to have my driver script run my tests I have created
#Need to make sure that I use known values from examples as arguments
#Also need to run 2 unit tests for each function
#First doing test.quadratic.determinant
#using same 3 coefficient values for each of the quadratic tests

#Need to include cat statements so that the output from bash terminal tells me which test is being run
#Adding cat("\n") after every test for legibility at the terminal


  cat("## Running quadratic.determinant a=1, b=0, c=-2 TEST:\n")

  test.quadratic.determinant(a=1, b=0, c=-2, 8, "quadratic.determinant, a=1, b=0, c=-2")
  cat("\n")

  cat("## Running quadratic.determinant a=2, b=-1, c=0 TEST:\n")

  test.quadratic.determinant(a=2, b=-1, c=0, 1, "quadratic.determinant, a=2, b=-1, c=0")
  cat("\n")

#Running test.quadratic.roots function with 2 different tests
#The tests vary by coefficient values

  cat("## Running quadratic.roots a=1, b=0, c=-2 TEST:\n")

  test.quadratic.roots(a=1, b=0, c=-2, 1.41421356237, -1.41421356237, "quadratic.root, a=1, b=0, c=-2")
  cat("\n")

  cat("## Running quadratic.roots a=2, b=-1, c=0 TEST:\n")

  test.quadratic.roots(a=2, b=-1, c=0, 0.5, 0, "quadratic.root, a=2, b=-1, c=0")
  cat("\n")

#Running test.quadratic.eqn function with 2 tests
#Again using same two coefficient value combinations with known results for test
  cat("## Running quadratic.eqn a=1, b=0, c=-2 TEST:\n")

  test.quadratic.eqn(a=1, b=0, c=-2, 8, 2, 2, "quadratic.equation, a=1, b=0, c=-2")
  cat("\n")

  cat("## Running quadratic.eqn a=2, b=-1, c=0 TEST:\n")

  test.quadratic.eqn(a=2, b=-1, c=0, 1, 4, 4, "quadratic.equation, a=2, b=-1, c=0")
  cat("\n")

#Running test.petit.pains.au.lait function with 2 tests
#using three peices and six pieces because they are easier and more clear than decimals


  cat("### Running petit.pain.au.lait 3 pieces TEST:\n")

  test.petit.pain.au.lait(3, c(1, .25, 3.75, .5, .5), "3 pieces petit.pain.au.lait")
  cat("\n")

  cat("### Running petit.pain.au.lait 6 pieces TEST:\n")

  test.petit.pain.au.lait(6, c(2, .5, 7.5, 1, 1), "6 pieces petit.pain.au.lait")
  cat("\n")
