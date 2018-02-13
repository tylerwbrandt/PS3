## PS 3

# S3 system
# Define doors
one <- 1
class(one) <- "door"
two <- 2
class(two) <- "door"
three <- 3
class(three) <- "door"

# Create PlayGame function for doors
PlayGame <- function(x){
  UseMethod("PlayGame", x)
}

PlayGame.door <- function(x){
  random_door <- sample(1:3, 1)
  if (x == random_door){
    print ("You won a new car!")
  } else {
    print ("Sorry, you will have to settle for a goat")
  }
}

PlayGame(one)

# S4 system
## Define Doors
setClass(Class = "door",
         representation = representation(
           x = "numeric"
         ),
         prototype = prototype(
           x = 1
         ))

setValidity("door", function(object){
  if (object @ x != 1 & object @ x != 2 & object@ x != 3){
    return ("@x value for door object must be in {1,2,3}")
  }
})

new("door", x = 4)
new("door", x = 3)
