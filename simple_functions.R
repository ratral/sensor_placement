#...............................................................................
# Function RESIDUAL VECTOR
#------------------
# The leak detection methodology is based on the computation of the 
# residual vector.  is the difference between the pressure measurements (p_i) 
# and its corresponding estimation (p_j) , obtained from the simulation of 
# the hydraulic model with no leak
#...............................................................................

residual_vector <- function(modelation, simulation, reading = "Pressure"){
  
  if(reading == "Pressure") {
    rv_1 <- modelation %>% select(ID, timeInSeconds, value = Pressure)
    rv_2 <- simulation %>% select(ID, timeInSeconds, value = Pressure)
    
  } else if(reading == "Flow") {
    rv_1 <- modelation %>% select(ID, timeInSeconds, value = Flow)
    rv_2 <- simulation %>% select(ID, timeInSeconds, value = Flow)
  } 
  
  residual_vector   <- left_join(rv_1, rv_2, 
                        by = c('ID','timeInSeconds')) %>%
                        mutate(Residual = abs(value.x - value.y)) %>%
                        select(ID, timeInSeconds, Residual)
  
  # residual_vector   <- left_join(simulation,
  #                                residual_vector, 
  #                                by = c('ID','Timestamp'))
}

#-------------------------------------------------------------------------------
# Modell * Resalts ID
#-------------------------------------------------------------------------------

result_id <- function(i){
  id_result <- ifelse ( i < 10,   "R0000", 
                        ifelse ( i < 100,   "R000", 
                        ifelse ( i < 1000,  "R00", 
                        ifelse ( i < 10000, "R0", "R"))))
  
  result_id <-  paste0(id_result, i)
} 
#-------------------------------------------------------------------------------
