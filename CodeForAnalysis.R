install.packages("readxl")

tinytex::install_tinytex()
# to uninstall TinyTeX, run
# tinytex::uninstall_tinytex()

library(readxl)
library(tinytex)

# Set the working directory to the directory where the project is located
setwd("your_path_to_data")
print(getwd())

# Create a relative path to the "data" folder in your project directory
data_folder <- file.path("data")
print(data_folder)

# Extract the absolute path from the relative path
absolute_path_to_data_folder <- normalizePath(data_folder)
print(absolute_path_to_data_folder)

file_name_node_list <- 'Node_List_Eurovision_public.xlsx'
file_name_edge_list<- 'Eurovision_Public.xlsx'


# Combine the data folder path and file name to create the full path
full_path_to_node_list <- file.path(data_folder, file_name_node_list)
print(full_path_to_node_list)

## read data from csv file
eurovision_public_data_node_list <- read_excel(full_path_to_node_list)
print(eurovision_public_data_node_list)

# Combine the data folder path and file name to create the full path
full_path_to_edge_list <- file.path(data_folder, file_name_edge_list)
print(full_path_to_edge_list)

eurovision_public_data_edge_list <- read_excel(full_path_to_edge_list)
print(eurovision_public_data_edge_list)

(NodeList_euro <- unique(c(eurovision_public_data_node_list$Node_country)))
NodeList_euro <- na.omit(NodeList_euro) # always remove NAs
class(NodeList_euro)

# select columns with network info as df
net_edge <- data.frame(eurovision_public_data_edge_list[,1:2])
net_edge <- na.omit(net_edge)

net_node <- data.frame(NodeList_euro) 
class(net_node)

eAttr <- data.frame(cbind(eurovision_public_data_edge_list$Score))
eAttr <- na.omit(eAttr)

# Make the edge list
EdgeList <- snafun::make_edgelist(net_edge, eAttr)
EdgeList <- na.omit(EdgeList)

nAttr <- eurovision_public_data_node_list$country_population
nAttr2 <- eurovision_public_data_node_list$country_language_family 
nAttr3 <- eurovision_public_data_node_list$country_government_system

class(nAttr)
print(nAttr)

# Make the node list for 1st attribute
NodeList <- snafun::make_nodelist(net_node, nAttr)
NodeList <- na.omit(NodeList)

# Make the node list for 2nd attribute
NodeList2 <- snafun::make_nodelist(net_node, nAttr2)
NodeList2 <- na.omit(NodeList2)

# Make the node list for 3rd attribute
NodeList3 <- snafun::make_nodelist(net_node, nAttr3)
NodeList3 <- na.omit(NodeList3)

# Convert matrices to data frames
df1 <- as.data.frame(NodeList)
df2 <- as.data.frame(NodeList2)
df3 <- as.data.frame(NodeList3)

# name columns
colnames(df1) <- c("node_country", "country_population")
colnames(df2) <- c("node_country", "country_language_family")  
colnames(df3) <- c("node_country", "country_government_system")

# Merge df1 and df2 based on the first column
merged_df <- merge(df1, df2, by = "node_country", all = TRUE)
print(merged_df)

# Merge the result with df3 based on the first column
merged_NodeList <- merge(merged_df, df3, by = "node_country", all = TRUE)

# Print the result
print(merged_NodeList)

eurovisionnet <- igraph::graph_from_data_frame(EdgeList, merged_NodeList, directed = TRUE)
plot(eurovisionnet)

snafun::extract_all_vertex_attributes(eurovisionnet)

## number of vertices
snafun::count_vertices(eurovisionnet)
## number_of_edges
snafun::count_edges(eurovisionnet)
## density 
snafun::g_density(eurovisionnet)
## reciprocity
snafun::g_reciprocity(eurovisionnet)
## transitivity
snafun::g_transitivity(eurovisionnet)
## mean_distance
snafun::g_mean_distance(eurovisionnet)
## number_of_isolates
snafun::find_isolates(eurovisionnet)
## dyad_census
snafun::count_dyads(eurovisionnet)
## triad_census
snafun::count_triads(eurovisionnet)

## GRAPH 1

# Calculate in-degrees
in_degrees <- igraph::degree(eurovisionnet, mode = "in")

vertex_size <- in_degrees * 1.2  
# Plot the graph with scaled vertex size

plot(
  eurovisionnet,
  edge.arrow.size = 0.15,
  edge.color = "gray80",
  vertex.frame.color = "#ffffff",
  vertex.label.cex = 0.6,
  vertex.label.color = "black",
  vertex.size = 20
)

snafun::plot_centralities(eurovisionnet)

# turn it into a network (is necessary for walktrap community)
print(class(eurovisionnet))
net_eurovision <- snafun::to_network(eurovisionnet)
print(class(net_eurovision))

## CUG test for detecting communities
walktrap_num_f <- function(x, directed = TRUE) { 
  x <- snafun::fix_cug_input(x, directed = directed)
  snafun::extract_comm_walktrap(x) |> length()
}
eurovision_coms <- sna::cug.test(net_eurovision, FUN = walktrap_num_f, mode = "graph",
                                 diag = FALSE, cmode = "dyad.census", reps = 1000)
print(eurovision_coms)

## Baseline ERGM model (prerequisities, must be a network object)
baseline_model_0.1 <- ergm::ergm(net_eurovision ~ edges)  
(s1 <- summary(baseline_model_0.1))

## Second model where we include reciprocity and we want to check whether the votes are reciprocated
baseline_model_0.2 <- ergm::ergm(net_eurovision ~ edges + mutual,
                                 control = ergm::control.ergm(MCMC.burnin = 5000,
                                           MCMC.samplesize = 10000,
                                           seed = 123456,
                                           MCMLE.maxit = 5))  
(s2 <- summary(baseline_model_0.2))

ergm::mcmc.diagnostics(baseline_model_0.2)

baseline_model_0.2_GOF <- ergm::gof(baseline_model_0.2)

## Note from the ERGM 3 lab
## I inserted a burn-in of 1000 and simulated 5000 networks. I used this set up to speed up computation within the tutorial. 
## You will need to increase these numbers to something like 10k and 40k (or more) when you will use these models for real!


## looks actually good, 
baseline_model_0.4 <- ergm::ergm(net_eurovision ~ edges + mutual + nodematch("country_government_system"),
                                 control = ergm::control.ergm(MCMC.burnin = 50000,
                                                              MCMC.samplesize = 100000,
                                                              seed = 123458,
                                                              MCMLE.maxit = 5))  
(s4 <- summary(baseline_model_0.4))

ergm::mcmc.diagnostics(baseline_model_0.4)

snafun::stat_ef_int(baseline_model_0.4, type = "odds")

snafun::stat_ef_int(baseline_model_0.4, type = "prob")




### that doesnt look that good :/ 
baseline_model_0.4_GOF <- ergm::gof(baseline_model_0.4)

snafun::stat_plot_gof(baseline_model_0.4_GOF)


## looks actually good, 
baseline_model_0.5 <- ergm::ergm(net_eurovision ~ edges + mutual + nodematch("country_language_family") + nodematch("country_government_system"),
                                 control = ergm::control.ergm(MCMC.burnin = 5000,
                                                              MCMC.samplesize = 10000,
                                                              seed = 123451,
                                                              MCMLE.maxit = 5))  
(s5 <- summary(baseline_model_0.5))

ergm::mcmc.diagnostics(baseline_model_0.5)


### that doesnt look that good :/ 
baseline_model_0.5_GOF <- ergm::gof(baseline_model_0.5)

snafun::stat_plot_gof(baseline_model_0.5_GOF)

## GRAPH 2
# Calculate total incoming weight for each vertex
in_weights <- sapply(igraph::V(eurovisionnet), function(v) {
  sum(igraph::E(eurovisionnet)[to(v)]$attribute)
})

# Scale the total incoming weight to set vertex size
vertex_size <- in_weights * 0.2  # Adjust the scaling factor as needed

plot(
  eurovisionnet,
  edge.arrow.size = 0.2,
  edge.color = "gray80",
  vertex.frame.color = "#ffffff",
  vertex.label.cex = 0.6,
  vertex.label.color = "black",
  vertex.size = vertex_size
)  

## GRAPH 3

# Calculate the minimum and maximum edge weights
min_weight <- min(igraph::E(eurovisionnet)$attribute)
max_weight <- max(igraph::E(eurovisionnet)$attribute)

# Define the colors for different weights
color_palette <- c("lightblue", "blue", "darkblue")  # Define colors for weights 8, 10, and 12

# Get the edge weights
edge_weights <- igraph::E(eurovisionnet)$attribute

# Map edge weights to colors
edge_colors <- sapply(edge_weights, function(weight) {
  # Define the color index based on the weight
  color_index <- match(weight, c(8, 10, 12))
  if (!is.na(color_index)) {
    return(color_palette[color_index])
  } else {
    return("black")  # Default color for unhandled weights
  }
})

# Plot the graph with adjusted edge colors
plot(
  eurovisionnet,
  edge.arrow.size = 0.1,
  edge.color = edge_colors,
  vertex.frame.color = "#ffffff",
  vertex.label.cex = 0.6,
  vertex.label.color = "black",
  vertex.size = vertex_size
)
# Create a legend
legend("topright", legend = c("8 points", "10 points", "12 points"), fill = color_palette, title = "Edge Weights")


## GRPAH 4

# Calculate the summary values for each vertex
summary_values <- sapply(igraph::V(eurovisionnet), function(vertex) {
  # Get the incoming neighbors for the vertex
  incoming_neighbors <- igraph::neighbors(eurovisionnet, vertex, mode = "in")
  
  # Calculate the summary value as the sum of incoming edge weights
  summary_value <- sum(igraph::E(eurovisionnet)[incoming_neighbors]$attribute)
  
  return(summary_value)
})

# Create vertex labels that include both the name and summary weight
vertex_labels <- sapply(igraph::V(eurovisionnet), function(vertex) {
  name <- igraph::V(eurovisionnet)$name[vertex]
  summary <- igraph::V(eurovisionnet)$summary_weight[vertex]
  label <- paste("Name:", name, "\nScore:", summary, sep = " ")
  return(label)
})

# Add the summary values as a new vertex attribute
igraph::V(eurovisionnet)$summary_weight <- summary_values
snafun::extract_vertex_attribute(eurovisionnet)
snafun::extract_edge_attribute(eurovisionnet)

# Plot the graph with customized vertex labels
plot(
  eurovisionnet,
  edge.arrow.size = 0.1,
  edge.color = edge_colors,
  vertex.frame.color = "#ffffff",
  vertex.label = vertex_labels,
  vertex.label.cex = 0.6,
  vertex.label.color = "black",
  vertex.size = vertex_size,
  vertex.label.dist = 2,  # Adjust label distance
  vertex.label.degree = 0  # Adjust label orientation
)
