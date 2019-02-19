#...............................................................................
# 1. - Initialize Session
#...............................................................................

  cat("\014")
  rm(list=ls()) 

  # Installs libraries 
  library(igraph)
  library(ggraph)
  library(tidyverse)  # tidy data analysis
  library(epanetReader)
  library(epanet2toolkit)
  library(jsonlite)

  # Paths / Directories

  paths <- list( data    =  file.path(getwd(),"data"),
                 reports = file.path(getwd(),"reports"))

  params       <- "base_dma_02.json"  # File name 

  params       <- file.path(paths$data,params)
  params       <- fromJSON(params, flatten=TRUE)

#...............................................................................
# 2. Read network information from an *.inp
#...............................................................................

  nw_input <- read.inp(file.path(paths$data,params$input_files))

#...............................................................................
# Running a Full Simulation
#...............................................................................

  ENepanet(file.path(paths$data,params$input_files), 
           file.path(paths$data,params$output_files$report))

  nw_report <- read.rpt(file.path(paths$data,params$output_files$report))

#...............................................................................
# Select nodes & pipes
#...............................................................................

  nodes <- as_tibble(nw_report$nodeResults) %>%
           group_by(ID, nodeType) %>%
           summarise(Pressure=mean(Pressure, na.rm = TRUE)) %>%
           ungroup()

  links <- nw_report$linkResults %>%
           group_by(ID, linkType) %>%
           summarise(Flow = mean(Flow, na.rm = TRUE))  %>%
           ungroup()

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

  
  # change from_node and to_node in function of the Flow direction !!
  
  links <- links %>% 
           mutate(temp = to,
                  to   = if_else(Flow < 0, from, to),
                  from = if_else(Flow < 0, temp, from),
                  Flow = if_else(Flow < 0, -Flow, Flow)) %>%
           select(-temp)
    
  remove(pipes)

#-------------------------------------------------------------------------------
# Generaiting the igraph object plus atributes
#
# nodes: The entities (nodes) to be connected in the network.
#        Synonyms: vertices of a graph.
# edges: The connections (interactions or relationships) between the entities. 
#        Synonyms: links, ties.
#
# adjacency matrix: a square matrix in which the column and row names are 
#                   the nodes of the network.
#
#-------------------------------------------------------------------------------
  
  # Create an igraph object with attributes directly from dataframes
  water_network <- graph_from_data_frame( d        = links, 
                                          vertices = nodes , 
                                          directed = TRUE)

#-------------------------------------------------------------------------------
# igraph objects
#-------------------------------------------------------------------------------
  # Subset vertices and edges
  V(water_network)
  E(water_network)
 
  # Count number of edges (Pippes, Valves, Pumps)
  gsize(water_network)
  
  number_of_edges <- links %>% 
                     group_by(linkType) %>%
                     summarise(length = sum(Length), n = n()) %>% 
                     ungroup()
  number_of_edges
  
  # Count number of vertices (Nodes, Tanks, Reservois)
  gorder(water_network)
  
  number_of_vertices <- nodes %>%
                        group_by(nodeType) %>%
                        summarise(n = n()) %>%
                        ungroup
  number_of_vertices
  
#-------------------------------------------------------------------------------
# Visualization
#-------------------------------------------------------------------------------
  
  # Plot the graph object water_network using ggraph's chosen layout 
  ggraph(water_network, layout = "with_kk") + 
    geom_edge_link(arrow = arrow(length = unit(1.5, 'mm')), 
                   start_cap = circle(2, 'mm'),
                   end_cap = circle(2, 'mm')) +
    geom_node_point(size = 3) 


#===============================================================================
# http://pablobarbera.com/big-data-upf/html/02b-networks-descriptive-analysis.html 
#-------------------------------------------------------------------------------
# NODE PROPERTIES :
# We’ll start with descriptive statistics at the node level. 
# All of these are in some way measures of importance or centrality.
#-------------------------------------------------------------------------------
  
  # Attributes
    vertex_attr(water_network)
  
  # Find all edges that include "JT_0I_011"
  
    E(water_network)[[inc("JT_0I_011")]]
  
  # Subset edges greater with negative flow

    E(water_network)[[Flow <= 0]] 
  

  #.............................................................................
  # DEGREE : 
  # The degree of a vertex is its most basic structural property, the number of
  # its adjacent edges. 
  # The most basic measure is degree, the number of adjacent edges to each node.
  # It is often considered a measure of direct influence. In directed graphs,
  # there are three types of degree: 
  #   - indegree (incoming edges), mode="in"
  #   - outdegree (outgoing edges), mode="out"
  #   - and total degree. mode="total"
  #.............................................................................
  
    degree(water_network, mode = "out")
    degree(water_network, mode = "in")
    degree(water_network, mode = "total")
  
  #.............................................................................
  # STRENGTH :
  # Strength is a weighted measure of degree that takes into account the number
  # of edges that go from one node to another. In this network, it will be the
  # total number of interactions of each character with anybody else.
  #.............................................................................
  
    strength(water_network)
  
  #.............................................................................
  # CLOSENES :
  # Closeness measures how many steps are required to access every other node 
  # from a given node. It’s a measure of how long information takes to arrive
  # (who hears news first?). Higher values mean less centrality.
  #.............................................................................
  
    closeness(water_network, normalized = TRUE)
  
  #.............................................................................
  # BETWEENNESS : 
  # measures brokerage or gatekeeping potential. It is (approximately) the 
  # number of shortest paths between nodes that pass through a particular node.
  # The vertex and edge betweenness are (roughly) defined by the number of
  # geodesics (shortest paths) going through a vertex or an edge. 
  # betweenness calculates vertex betweenness, 
  # edge_betweenness calculates edge betweenness.
  # 
  #.............................................................................
  
    betweenness(water_network, directed = TRUE)
    edge_betweenness(water_network)
  
  
  #.............................................................................
  # EIGENVECTOR CENTRALITY :
  # Eigenvector centrality is a measure of being well-connected connected to the
  # well-connected. First eigenvector of the graph adjacency matrix.
  # Only works with undirected networks.
  #.............................................................................
  
    eigen_centrality(water_network)$vector
  
  #.............................................................................
  # PAGE RANK:
  # Page rank approximates probability that any message will arrive to a 
  # particular node. This algorithm was developed by Google founders, and 
  # originally applied to website links.
  #.............................................................................
  
    page_rank(water_network)$vector
  
  #.............................................................................
  # AUTORITY SCORE:
  # Authority score is another measure of centrality initially applied to the Web.
  # A node has high authority when it is linked by many other nodes that are 
  # linking many other nodes.
  #.............................................................................
  
    authority_score(water_network)$vector
  
  #.............................................................................
  # NEIGHBORS and EGO:
  # Finally, not exactly a measure of centrality, but we can learn more about 
  # who each node is connected to by using the following functions: 
  #   - neighbors (for direct neighbors) and 
  #   - ego (for neighbors up to n neighbors away)
  #.............................................................................
  
    neighbors(water_network, v=which(V(water_network)$name=="JT_0D_007"))
  
    ego(water_network, order=2, nodes=which(V(water_network)$name=="JT_0D_007"))
  
 #=============================================================================
 # http://pablobarbera.com/big-data-upf/html/02b-networks-descriptive-analysis.html
 #-----------------------------------------------------------------------------
 # NETWORK PROPERTIES :
 #..............................................................................
 # Let’s now try to describe what a network looks like as a whole. We can start
 # with measures of the size of a network. 
 # diameter is the length of the longest path (in number of edges) between two
 # nodes. We can use get_diameter to identify this path. mean_distance is the
 # average number of edges between any two nodes in the network.
 # We can find each of these paths between pairs of edges with distances.
 #------------------------------------------------------------------------------
  
  #.............................................................................
  # DIAMETER:
  # The diameter of a graph is the length of the longest geodesic. 
  #.............................................................................
  
    diameter(water_network,          directed = TRUE, weights=NA)
  
    get_diameter(water_network,      directed = TRUE, weights=NA)
  
  
  #.............................................................................
  # MEAN DISTANCE :Average Path Length
  # mean_distance calculates the average path length in a graph, by calculating 
  # the shortest paths between all pairs of vertices (both ways for directed graphs). 
  # This function does not consider edge weights currently and uses a 
  # breadth-first search. 
  #.............................................................................
  
    mean_distance(water_network,     directed = TRUE)
  
    farthest_vertices(water_network, directed = TRUE)
  
  #.............................................................................
  # DENSITY : 
  # Density is the proportion of observed links in a network to the maximum 
  # number of possible links. Thus, density is a ratio that can range from 0 to 1.
  # The closer to 1 the density is, the more interconnected is the network.
  # The density of a graph is the ratio of the number of edges and the number
  # of possible edges. 
  # edge_density is the proportion of edges in the network over all possible
  # edges that could exist.
  #.............................................................................
  
    edge_density(water_network)

  #.............................................................................
  # RECIPROCITY:
  # reciprocity measures the propensity of each edge to be a mutual edge; 
  # that is, the probability that if i is connected to j, j is also connected to i.
  #.............................................................................
  
    reciprocity(water_network)
  
  #.............................................................................
  # TRANSITIVITY :
  # transitivity, also known as clustering coefficient, measures that probability 
  # that adjacent nodes of a network are connected. In other words, if i is 
  # connected to j, and j is connected to k, what is the probability that i is
  # also connected to k?
  #.............................................................................
  
    transitivity(water_network)

#=============================================================================
# http://pablobarbera.com/big-data-upf/html/02b-networks-descriptive-analysis.html
#-----------------------------------------------------------------------------
# NETWORK COMMUNITIES :
#..............................................................................
# Networks often have different clusters or communities of nodes that are more 
# densely connected to each other than to the rest of the network. 
# Let’s cover some of the different existing methods to identify these communities.
#
# The most straightforward way to partition a network is into connected components.
# Each component is a group of nodes that are connected to each other, but not 
# to the rest of the nodes.
#------------------------------------------------------------------------------

        
