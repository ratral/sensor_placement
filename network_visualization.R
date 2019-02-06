#...............................................................................
# 1. - Initialize Session                                                  #### 
#...............................................................................

cat("\014")
rm(list=ls()) 

# Installs libraries 
library(tidyverse)
library(tidygraph)
library(epanetReader)
library(epanet2toolkit)
library(jsonlite)

paths <- list( data =  file.path(getwd(),"data"),
               reports = file.path(getwd(),"reports"))

params       <- "base_dma_02.json"  # File name 

params       <- file.path(paths$data,params)
params       <- fromJSON(params, flatten=TRUE)


#...............................................................................
# 2. Read network information from an *.inp
#...............................................................................

nw_input <- read.inp(file.path(paths$data,params$input_files))

#...............................................................................
# 3. Running a Full Simulation                                             ####
#...............................................................................

ENepanet(file.path(paths$data,params$input_files), 
         file.path(paths$data,params$output_files$report))

nw_report <- read.rpt(file.path(paths$data,params$output_files$report))

#...............................................................................
# Select nodes & pipes
#...............................................................................

nodes <- as_tibble(nw_report$nodeResults) %>%
         group_by(ID, nodeType) %>%
         summarise(Pressure=mean(Pressure, na.rm = TRUE))

links <- nw_report$linkResults %>%
         group_by(ID, linkType) %>%
         summarise(Flow = mean(Flow, na.rm = TRUE)) 

#...............................................................................

pipes  <- as_tibble(nw_input$Pipes) %>% 
          select(ID, Node1,Node2, Length, Diameter)

if (!is.null(nw_input$Valves)){
  
  temp_tab <- as_tibble(nw_input$Valves) %>%
              select(ID, Node1,Node2, Diameter) %>%
              mutate(Length = NA) %>%
              select(ID, Node1, Node2, Length, Diameter)
  
  pipes <- bind_rows(pipes, temp_tab)
  
}

if (!is.null(nw_input$Pumps)){

  temp_tab <- as_tibble(nw_input$Pumps) %>%
              select(ID, Node1, Node2) %>%
              mutate(Length = NA, Diameter = NA) %>%
              select(ID, Node1, Node2, Length, Diameter)
  pipes <- bind_rows(pipes, temp_tab)
}

links <- full_join(links, pipes, by = "ID") %>% 
         select(Node1, Node2, Length, ID, Diameter, Flow, linkType)

links <- links %>% rename(from = "Node1", to ="Node2")

remove(pipes)

#-------------------------------------------------------------------------------
# The tbl_graph object
#-------------------------------------------------------------------------------

water_network <- tbl_graph(nodes = nodes, edges = links, directed = TRUE )

#-------------------------------------------------------------------------------

# Attributes

vertex_attr(water_network)

edge_attr(ater_network) #!!!

# Visualization

plot( water_network, 
      layout = layout_with_graphopt, 
      edge.arrow.size = 0.2)
