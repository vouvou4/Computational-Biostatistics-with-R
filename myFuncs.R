
#creating myFuncs.R functions

#Creating checkArgsQuad function which is my defensive function for all my quadratic equations
#This function must take the arguments which are given to all the quadratic functions
#Making sure that all coefficients given to the function are numeric values

checkArgsQuad <- function(a, b, c){

  if(!is.numeric(a)) {
    stop("Coefficient a in the quadratic determinant must be a numeric value.")
  }

  if(!is.numeric(b)) {
    stop("Coefficient b in the quadratic determinant must be a numeric value.")
  }

  if(!is.numeric(c)) {
    stop("Coefficient c in the quadratic determinant must be a numeric value.")
  }

}

###############################################################

#First creating quadratic.determinant function to calculate and return determinant denoted by det
#This function is taking the arguments of the coefficients of the quadratic formula
#Added in the checkArgsQuad() which is my defensive function to make sure arguments are functions

quadratic.determinant <- function(a, b, c) {

  checkArgsQuad(a, b, c)

    det <- b^2 - (4 * a * c)

    return(det)
}

###############################################################
#Creating another defensive function that I can call in quadratic.roots and quadratic.eqn so that my code is not redundant
#also creating this function so that code is not too long
#here I will make sure that a==0 then the code stops because this would give undefined value
#next making sure that the determinant is positive because cant take the square root of a negative number

  check.determ.roots <- function(a, det) {
    if(det < 0) {
      stop("The determinant must be greater than 0 as the square root of a negative number is not possible. Please choose differnt values.")
    }

    if(a==0) {
      stop("Quadratic function cannot process a = 0 as the output is undefined. Please choose a new value")
    }
  }

###############################################################

#Making quadratic.roots function where  I can calculate and return the roots
#Have to create two variables for the + and - condition of quadratic equation
#list at end will print out the roots in an un-named list
#calling on quadratic.determinant() function from above
#Remember to pass your functions the right arguments when you are calling on your function above - they cant run with just()
# Again added in the checkArgsQuad() which is my defensive function to make sure arguments are functions
#Here added the determminant check where determinant must be greater than 0 or else stop running

quadratic.roots <- function(a, b, c) {

    checkArgsQuad(a, b, c)

    det <- quadratic.determinant(a, b, c)

    check.determ.roots(a, det)

    rootsplus <- (-b + sqrt(det)) / (2 * a)
    rootsminus <- (-b - sqrt(det)) / (2 * a)
    return(list(rootsplus, rootsminus))
}


###############################################################

#Making quadratic.eqn and defining the argument values if they are not specified
#first defining my variables for the master list - note all of these are lists themselves
#call upon my functions I previously defined in this file using function(arguments)
#then make the master list where I make a named list so I can search through the list with $name ex. $eqn to see the coefficients
#added checkArgsQuad() here again for defensive precaution
#again add the determ<0 check which will cause function to stop running

quadratic.eqn <- function(a= 1, b= 0, c= 0) {

    checkArgsQuad(a, b, c)

   coeff <- list(a = a, b = b, c = c)
   det <- quadratic.determinant(a, b, c)
   roots <- quadratic.roots(a, b, c)
   first.deriv <- list(2*a, b)
   second.deriv <- 2*a

   check.determ.roots(a, det)

   return(list(eqn = coeff,
              determinant = det,
              roots = roots,
              first.deriv = first.deriv,
              second.deriv = second.deriv))
 }

###############################################################

#Creating the petit.pain.au.lait function, takes argument quant referring to the quantity of pieces you want, default = 12
#I first define serving_size which will be the argument given to function /12 the original serving size
#I am creating all of my vectors which I will combine into a data frame later
#These vectors represent the amounts, ingredients, and units for the recipe for 12 servings
#create data.frame where amounts is multiplied by serving size in the data frame output
#lastly print the recipe, note cat cannot handle printing out a data.frame
#Added some defensive coding to this function. First writing ti make sure that the serving size(argument) is always a numeric
#next making sure that the serving size is always positive

petit.pains.au.lait <- function(quant = 12){

    if(!is.numeric(quant)){
      stop("The serving size must be a numeric value.")
    }

    if(quant <= 0) {
      stop("The value of the serving size must be postive and greater than 0. Please choose a new serving size.")
    }

      serving.size <- quant/12
      ingredients <- c("Flour", "Milk", "Yeast", "Salt", "Sugar")
      amounts <- c(4, 1, 15, 2, 2)
      units <- c("Cups", "Cups", "Grams", "Tbsp", "Tbsp")
      recipe <- data.frame(Ingredients = ingredients, Amounts = (amounts*serving.size), Units = units)
      print(recipe)
      return(recipe)
  }
