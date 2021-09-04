#' Create the orginization of goats and cars.
#' 
#' @param goats Numer of goats that will be hidden behind doors.
#' @param cars Number of cars that will be hidden behind doors.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return create_game() will create a sample game of two goats and one car by default, 
#' but also will allow the user to define the number of goats and cars in the simulation.
#' 
#' @examples
#' ## Randomly sorts 2 goats and 1 car
#' create_game()
#' 
#' ## Randomly sorts 4 goats and 3 cars.
#' create_game(4, 3)
#' 

#' @export
create_game <- function( goats = 2, cars = 1 )
{
  a.game <- sample( x=c(rep("goat",goats), rep("car", cars)), size = (goats + cars), replace=F )
  return( a.game )
}