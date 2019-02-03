#...............................................................................
# 1. - Initialize Session                                                  #### 
#...............................................................................

cat("\014")
rm(list=ls()) 

# Installs libraries 
library(tidyverse)
library(dplyr)
library(zoo)
library(lubridate)
library(epanetReader)
library(epanet2toolkit)
library(ggfortify)
library(ggthemes)
library(scales)
library(purrr)
library(visNetwork)

# Initialize params
params <- list(base_network    = "base_dma_02", 
               new_network     = "base_dma_w_leaks",
               functs_name     = "epanet_api_functions",
               inlet_valves    = "PRV_",
               jt_to_analyze   = "^JT_0[A-K]", # RegExp
               pipe_to_analyze = "PS_",        # RegExp
               leak_rate       = 0.01, # Percentage of the network with leaks
               demad_factor    = list( names =c( "wd_spring_summer",
                                                 "hw_spring_summer",
                                                 "wd_summer_break",
                                                 "hw_summer_break",
                                                 "wd_fall_winter",
                                                 "hw_fall_winter"),
                                       factors = c( 0.92, 1.00, 1.09, 
                                                    0.81, 0.66, 0.95)),
               work_folders   = list( dir_work   = getwd(),
                                      dir_report = file.path(getwd(),"reports"),
                                      dir_data   = file.path(getwd(),"data"),  
                                      dir_bin    = file.path(getwd(),"reports"),
                                      dir_func   = file.path(getwd(),"func")))

params$f_names <- list( base_file_inp    = file.path(params$work_folders$dir_data, 
                                                     paste0(params$base_network,".inp")),
                        base_file_report = file.path(params$work_folders$dir_report, 
                                                     paste0(params$base_network,".rpt")),
                        new_file_inp     = file.path(params$work_folders$dir_data, 
                                                     paste0(params$new_network,".inp")),
                        new_file_report  = file.path(params$work_folders$dir_report,
                                                     paste0(params$new_network,".rpt")),
                        file_func        = file.path(params$work_folders$dir_func, 
                                                     paste0(params$functs_name,".R")))


# Load Functions Standard
source(params$f_names$file_func)

# load model
net_input_01  <- read.inp(params$f_names$base_file_inp)

# initialise th Juction for the leakage analyse
junctions <- net_input_01$Junctions %>%
             filter(grepl(params$jt_to_analyze,ID))

# initialise data frames
leak_node <- tibble( "ID"       = character(),
                     "FlowCoef" = double(),
                     "leakflow" = double())




# Generate Leack
emitters_ID <- junctions$ID[sample(1:nrow(junctions),1)]


# coef ~= y = 0.1436*(l/s) - 0.0026
#   Leack(l/s) = 2.0	then coef =  0.285

coefficient <- 0.2846

net_input_01$Emitters <- data.frame(ID = emitters_ID, FlowCoef = coefficient)

write.inp(net_input_01, params$f_names$new_file_inp)

rm(net_input_01)

#...............................................................................
# 3. Running a Full Simulation                                             ####
#    The function ENepanet() runs a full simulation and 
#    writes the results to a file. 
#...............................................................................

ENepanet(params$f_names$base_file_inp, 
         params$f_names$base_file_report)

ENepanet(params$f_names$new_file_inp,
         params$f_names$new_file_report)

net_input_01  <- read.inp(params$f_names$new_file_inp)
report_base   <- read.rpt(params$f_names$base_file_report)
report_leack  <- read.rpt(params$f_names$new_file_report)


# 4.1.- the average inflow f(t) was calculated at each hour t

base_inletflow <- inlet_flows(report_base,  params$inlet_valves, group = FALSE) 
leak_inletflow <- inlet_flows(report_leack, params$inlet_valves, group = FALSE)

inletflow      <- full_join(base_inletflow, leak_inletflow, 
                            by = "timeInSeconds") %>%
                  select(timeInSeconds, inflow.x, inflow.y)  %>%
                  mutate(leakflow = inflow.y - inflow.x)

inletflow <- median(inletflow$leakflow)

#-----
leak_node <- add_row(leak_node, "ID"       = emitters_ID, 
                                "FlowCoef" = coefficient, 
                                "leakflow" = inletflow)
#-------------------------


rm(base_inletflow,leak_inletflow)

# residual vector

rep01 <- eval_nodes (report_base, node_type = "",
                     id_nodes  = params$jt_to_analyze, 
                     group = FALSE)

rep02 <- eval_nodes (report_leack, node_type = "",
                     id_nodes  = params$jt_to_analyze, 
                     group = FALSE)

residual_vector <- full_join(rep01, rep02, by = c("timeInSeconds","ID")) %>%
                   mutate(D_Pressure = Pressure.y - Pressure.x) %>%
                   select(timeInSeconds,ID, D_Pressure) %>%
                   group_by(ID) %>%
                   summarise( rv_min = min(D_Pressure))

residual_matrix <- 1

rm(rep01,rep02)

# sensitivity vector

sensitivity_vector <- residual_vector %>% 
                      mutate(sensitivity = rv_min/inletflow)
                      

# detection capability matrix Mdc Where : 
# - the rows represent the pontential sensor locations and 
# - the columns represent the potential leak location




