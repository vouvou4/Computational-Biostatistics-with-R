#Creating Infection.Utiities.R file

#First Creating inital.pop.infection.vec function
#This takes 2 arguments n which is the size of the population, and k the number of initally-infected people
#Infected person is coded by 1 and healthy person coded as 0
#Function returns a vector of integers
#Here I assign my population, denoted by n to init.population and I code them all as initally healthy (0)
#Next I use sample() to randomly select the number of people who are infected denoted by k
#note in sample have to use seq to tell R that all the 0's is a vector and to select random individuals from this vector
#next I slice the init.population by the randomly generated infected individuals and assign those people as 1
#finally it returns my inital population n with k randomly infected individuals

  inital.pop.infection.vec <- function(n, k){

    init.population <- rep(0, n)

    infected.individ <- sample(seq(init.population), k)

    init.population[infected.individ] <- 1

    return(init.population)

  }

######################################################
#creating prob.of.transmission() which will describe the probability of infecting someone based on association with mask wearing
#takes argument n, the population size
#takes argument prop.mask.wear which is the proportion of mask wearing for each category (N95, non-medical, no mask) as optional argument
#here using sample again to let function choose from the three probabilities associated with mask wearing
#next sample uses the probabilites from prop.mask.wear to determine proportion of the population associated with that mask wearing behaviour
#lastly it returns a vector of probabilities associated with each mask wearing behaviour of length n (population size)
#replace= TRUE allows sample to sample from sample larger than population, needed when giving value as argument
#pop.prob.vec is the variable for the vector of population probabilites
#0.01 is prob of infecting someone when wearing N95, 0.05 when wearing non-medical mask, 0.1 when no mask

  prob.of.transmission.vec <- function(n, prop.mask.wear = c(1/3, 1/3, 1/3)){

      pop.prob.vec <- sample(c(0.01, 0.05, 0.1), n, replace = TRUE, prob = prop.mask.wear)

      return(pop.prob.vec)

  }

######################################################
#Creating the pop.interaction.matrix to have a matrix of interaction for population
#first randomly assign the population to 0 or 1 with equal probability
#this assignment represents either 1(the 2 people interact) or 0(no interaction)
#next use this random generation to create a matrix of n x n by making row and col = n
#then make the diagonal of matrix = 0
#next use the t() to create a symmetrical matrix
#finally return the matrix created

  pop.interaction.matrix <- function(n){

      random.chance.of.interact <- sample(c(0,1), n*n, replace = TRUE)

      matrix.of.interact <- matrix(random.chance.of.interact, nrow=n, ncol=n)

      diag(matrix.of.interact) <- 0

      matrix.of.interact[lower.tri(matrix.of.interact)] <- t(matrix.of.interact)[lower.tri(matrix.of.interact)]

      return(matrix.of.interact)

  }

######################################################
#creating my iteration.func() to loop over non infected individuals and determine if they become infected or not
#First need to assign my current.infect.stat to a copy variable that will be returned at the end
#Doing this because if I do current.infect.stat and assign that to 1 if infected in the for loop its going to overwrite each person
#First created a for loop, to loop over each sequence or position in my current.infect.stat vector
#next have if() to select only people who are healthy initally to determine if they will get sick
#next inside if statement use prob.row as my probability row for that specific person by multiplying their row in matrix, probabilities, their probabiliites and infection status of everyone
#now can use sapply to determine randomly if the persons interactions with the others will result in infection based on probabilities above
#next use if(sum) to see if the sum of that randomly assigned row of probabilities is greater than 0
#if greater than 0(infection) then assign that person as 1 in my copy vector
#at the end return my copy vector

  iteration.func <- function(current.infect.stat, infect.prob.vec, matrix.interact){

    current.infect.stat.copy <- current.infect.stat

    for(i in seq_along(current.infect.stat)){

            if(current.infect.stat[i]==0){

              prob.row <- matrix.interact[i,]*infect.prob.vec*infect.prob.vec[i]*current.infect.stat

              random.assign <- sapply(prob.row, function(prob.row) (sample(c(1,0), 1, replace = FALSE, prob=c(prob.row, 1-prob.row))))

              if(sum(random.assign)>0){

                current.infect.stat.copy[i] <- 1

              }

            }

          }

    return(current.infect.stat.copy)
  }


######################################################
#creating iteration.num.func
#first takes 4 arguments, ones defined above because will use iteration.func and num representing number of times to iterate
#next assign the current.infect.stat to a variable because we never want to update our arguments
#also assigning the num of times to a vector
#using a for loop to loop over each number of interation
#Assigning the iteration function to the new.vector.of.cases which represents that new infection status vector
#then sum all the cases for that vector(remember they are 0 and 1 so just summing all 1s or infections)
#next update the num vector by slicing the position based on loop and assign it the value of summed cases
#note: because I assign my new.current.infect.stat to the iteration.func() this will automatically update for each iteration it does
#finally return the total cases for each iteration +1 for first case which represents the first sick person


  iteration.num.func <- function(current.infect.stat, infect.prob.vec, matrix.interact, num){

    new.current.infect.stat <- current.infect.stat

    output.vector <- (1:num)

    for(i in output.vector){

      new.current.infect.stat <- iteration.func(new.current.infect.stat, infect.prob.vec, matrix.interact)

      total.cases.per.iteration <- sum(new.current.infect.stat)

      output.vector[i] <- total.cases.per.iteration

    }

    return(c(1, output.vector))
  }
