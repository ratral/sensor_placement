#...............................................................................
# Initialize Session
#...............................................................................

cat("\014")
rm(list=ls()) 

#...............................................................................
# Installs libraries 
#...............................................................................
library(tidyverse)
library(epanetReader)
library(epanet2toolkit)

#...............................................................................
# Initialize params
## coefficient ~= y = 0.1436*(l/s) - 0.0026 
##   Leack(l/s) = 2.0	then coef =  0.285
#...............................................................................

model_files <- list( network        = "./data/base_dma_02.inp",
                     temp_network   = "./data/temp_network.inp",
                     report         = "./reports/rep_dma_02.rpt",
                     temp_report    = "./reports/temp_report.rpt",
                     results_rds    = "./data/network_results.rds",
                     components_rds = "./data/network_components.rds")

params <- list( id_model  = "M00000",
                id_result = "R00000",
                nodes_to_analyze   = "^JT_0[A-K]", # RegExp
                pipes_to_analyze   = "^PS_",       # RegExp
                inlets_and_outlets = "^PRV_",      # RegExp
                coefficient = 0.2846) 

#...............................................................................
# Sources
#...............................................................................
source("./func/simple_functions.R")


#...............................................................................
# Read Data Network Base
#...............................................................................
networks <- read.inp(model_files$network)

#...............................................................................
# Calculation and generating Report Base
#...............................................................................
ENepanet(model_files$network, model_files$report)

#...............................................................................
#Read Report
#...............................................................................
report  <- read.rpt(model_files$report)

#...............................................................................
## Generate Network Components and Results of the Network BASE
#...............................................................................

network_components <- tibble( id_model = params$id_model,
                              junctions = list(as_tibble(networks$Junctions)),
                              reservoirs = list(as_tibble(networks$Reservoirs)),
                              tanks = list(as_tibble(networks$Tanks)),
                              pipes = list(as_tibble(networks$Pipes)),
                              pumps = list(as_tibble(networks$Pumps)),
                              valves = list(as_tibble(networks$Valves)),
                              coordinates = list(as_tibble(networks$Coordinates)))

network_results    <- tibble( id_model  = params$id_model,
                              id_result = params$id_result,
                              emitters = list(as_tibble(networks$Emitters)),
                              node_results = list(as_tibble(report$nodeResults)),
                              link_results = list(as_tibble(report$linkResults)),
                              residual_pressure = NA,
                              residual_flow = NA,
                              leak_size = NA,
                              sensitivity_pressure = NA,
                              sensitivity_flow = NA,
                              sensor_pressure = NA,
                              sensor_flow = NA)

remove(report)

#...............................................................................
# Calculation of the pipe length associated each the nodes.
#...............................................................................

node1 <- network_components$pipes[[1]] %>% 
         select(Node1, Node2, Length) %>% 
         group_by(Node1) %>% 
         summarize(sum(Length)) %>% 
         rename(ID = Node1, sum1 = `sum(Length)`)

node2 <- network_components$pipes[[1]] %>% 
         select(Node1, Node2, Length) %>% 
         group_by(Node2) %>% 
         summarize(sum(Length)) %>% 
         rename(ID = Node2, sum2 = `sum(Length)`)

node  <- full_join(node1, node2, by = "ID") %>% 
         replace_na(list(sum1 = 0, sum2 = 0)) %>% 
         mutate(Length = (sum1+sum2)/2) %>%
         select(ID, Length) 

network_components$node_pipe_length <- list(node)

remove(node1, node2, node)

#...............................................................................
# Generate Models with Single-Leacks Scenario assumption !!
#...............................................................................
      

nodes <- network_components$junctions[[1]]
nodes <- nodes %>% subset(grepl(params$nodes_to_analyze,ID)) %>% select(ID)
nodes <- nodes$ID

# Loop

for (i in 1:length(nodes)) {

  networks$Emitters <- data.frame(ID = nodes[i], 
                                  FlowCoef = params$coefficient)
  
  write.inp(networks, model_files$temp_network)

  ENepanet(model_files$temp_network, model_files$temp_report)

  #Read Report

  report  <- read.rpt(model_files$temp_report)

  network_results <- network_results %>%
                     add_row( id_model     = params$id_model,
                              id_result    = result_id(i),
                              emitters     = list(as_tibble(networks$Emitters)),
                              node_results = list(as_tibble(report$nodeResults)),
                              link_results = list(as_tibble(report$linkResults)))
}

remove(nodes, networks, report) # !! REMOVE

#-------------------------------------------------------------------------------
#  calculation RESIDUALS of pressure and flow
#-------------------------------------------------------------------------------

for(i in 1:length(network_results$id_result)) {
  
  d_pressure <- residual_vector( network_results$node_results[[1]],
                                 network_results$node_results[[i]],
                                 reading = "Pressure")
  
  d_flow    <- residual_vector( network_results$link_results[[1]],
                                network_results$link_results[[i]],
                                reading = "Flow")
  
  network_results$residual_pressure[i] <- list(d_pressure)
  network_results$residual_flow[i]     <- list(d_flow)
  
}

remove(i,d_pressure,d_flow)  # !! REMOVE

#-------------------------------------------------------------------------------
# calculation of the leak_size
#-------------------------------------------------------------------------------

leak_size_01 <- network_results$link_results[[1]] %>% 
                subset(grepl(params$inlets_and_outlets,ID)) %>%
                select(timeInSeconds, Flow ) %>%
                group_by(timeInSeconds) %>%
                summarize(Flow = sum(Flow))

for(i in 1:length(network_results$id_result)) {
  

  leak_size  <- network_results$link_results[[i]] %>% 
                subset(grepl(params$inlets_and_outlets,ID)) %>%
                select(timeInSeconds, Flow ) %>%
                group_by(timeInSeconds) %>%
                summarize(Flow = sum(Flow))
  
  leak_size <- full_join(leak_size_01, leak_size, by = "timeInSeconds") %>%
               mutate(LeakFlow = Flow.y - Flow.x) %>%
               select(timeInSeconds, LeakFlow)
  
  
  network_results$leak_size[i] <- list(leak_size)

}

remove(i,leak_size_01,leak_size)  # !! REMOVE

#-------------------------------------------------------------------------------
# 2. - calculation of the sensitivity Matrix
#-------------------------------------------------------------------------------

for(i in 2:length(network_results$id_result)) {
  
  sensitivity_pressure <-  left_join(network_results$residual_pressure[[i]],
                                     network_results$leak_size[[i]],
                                     by = "timeInSeconds") %>%
                           mutate(Sensitivity = Residual/LeakFlow) %>% 
                           select(ID, timeInSeconds, Sensitivity)
  
  sensitivity_flow <-  left_join(network_results$residual_flow[[i]],
                                 network_results$leak_size[[i]],
                                 by = "timeInSeconds") %>%
                       mutate(Sensitivity = Residual/LeakFlow) %>% 
                       select(ID, timeInSeconds, Sensitivity)
  
  network_results$sensitivity_pressure[i] <- list(sensitivity_pressure)
  network_results$sensitivity_flow[i]     <- list(sensitivity_flow)
  
}

remove(i,sensitivity_pressure, sensitivity_flow)  # !! REMOVE

#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------

for(i in 2:length(network_results$id_result)) {
  
  sensor_pressure <- left_join(network_results$residual_pressure[[i]],
                               network_results$sensitivity_pressure[[i]],
                               by = c('ID','timeInSeconds')) %>%
                     mutate(ID_leak  = network_results$emitters[[i]]$ID) %>%
                     select(ID, timeInSeconds , Residual, Sensitivity, ID_leak)
  
  
  sensor_flow <- left_join(network_results$residual_flow[[i]],
                           network_results$sensitivity_flow[[i]],
                           by = c('ID','timeInSeconds')) %>%
                 mutate(ID_leak  = network_results$emitters[[i]]$ID) %>%
                 select(ID, timeInSeconds , Residual, Sensitivity, ID_leak)
  
  network_results$sensor_pressure[i] <- list(sensor_pressure)
  network_results$sensor_flow[i]     <- list(sensor_flow)
  
}

remove(i,sensor_pressure, sensor_flow) 

#-------------------------------------------------------------------------------
# Save the DB of the calculation
#-------------------------------------------------------------------------------
saveRDS(network_components,model_files$components_rds)
saveRDS(network_results,model_files$results_rds)

cat("\014")
rm(list=ls()) 

#-------------------------------------------------------------------------------
