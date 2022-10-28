#Creating Infection.Analysis.R

#source Utilities file

    source("Infection.Utilities.R")

#Create command line args code

    mask.status <- commandArgs(trailingOnly = TRUE)

#Add defensive coding here so use correct arguments at bash terminal
#Protecting against arguments that are not characters, too many arguments, or no argument presented

    if(length(mask.status) > 1){
      stop("Too many arguments presented. Choose either Masked or Unmasked")
    }

    if(length(mask.status)==0){
      stop("Plase enter a valid argument at the command line. Argument is either Masked or Unmasked")
    }

#Create code so when "Masked" is put to command line use fraction c(0.15, 0.7, 0.15)
#Cats masked propotions vector to screen
#Does similar function for "Unmasked" argument

    if(mask.status == "Masked"){
      prop.mask.wear.new <- c(0.15, 0.7, 0.15)
      cat("Using the masked proportions vector.", "\n")
    }

    if(mask.status == "Unmasked"){
      prop.mask.wear.new <- c(0.15, 0.5, 0.35)
      cat("Using the unmasked propotions vector.", "\n")
    }





#Create an inital population of 500 people with only 1 Infection using inital.pop.infection.vec()
#this is being assigned to a variable which I will pass to iteration.func()

    current.infect.stat <- inital.pop.infection.vec(500, 1)

#Create vector of infection probabilities based on the vector of mask wearers defined above
#Note the prop.mask.wear value will be determined based on command line argument

    infect.prob.vec <- prob.of.transmission.vec(500, prop.mask.wear.new)

#Create the interaction matrix for the population of 500

    matrix.interact <- pop.interaction.matrix(500)

#Iterates through the person-person interactions 20 times
#prints out the number of infected people at the end of each iteration to screen

    cat("The number of infected people, for 20 iterations, is:", "\n")
    total.iteration.values <- iteration.num.func(current.infect.stat, infect.prob.vec, matrix.interact, 20)
    print(total.iteration.values)




#Difference between two cases:
#Because the  Masked case has a higher proportion wearing a non-medical mask the probability
#of people in that population becoming sick (1) is lower because the highest probability (0.1) was associated with no mask
#In terms of the output this results in people getting sick slower throughout the 20 interations
#For the Unmasked case they have a higher probability of getting sick, and this is reflected by the iterations because
#this run has more people getting sick faster or earlier on in the output vector(higher numbers of infection earlier) as well as higher cases overall
#The similarities between the two is that once a few people get sick, the values for every iteration after increase almost exponentially and
#they also both end around the same amount of people in the population getting sick (in the 400 range)
#I would have changed the model by altering the propotions of getting sick because COVID-19 is easily transimissible so possibly having a higher prob.of.transmission
#as well as in the masked condition a higher proportion of people wearing non-medical masks, hopefully this should be close to 99%
#of our current society when going out and interacting with other people. Therefore the masked propotion would be about c(0.15, 0.84, 0.01)
#where 0.15 is N95 mask wearers, 0.84 is non medical mask wearers and hopefully only 1% is no mask 
