install.packages("readxl")
library(readxl)

# Set the working directory to the directory where the project is located
setwd("your_path_to_project")
print(getwd())

# Create a relative path to the "data" folder in your project directory
data_folder <- file.path("data")
print(data_folder)

# Extract the absolute path from the relative path
absolute_path_to_data_folder <- normalizePath(data_folder)
print(absolute_path_to_data_folder)

file_name_node_list <- 'Node_List_Eurovision_public.xlsx'
file_name_edge_list<- 'name_eurovision_edge_list.xlsx'

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
class(nAttr)

# Make the node list
NodeList <- snafun::make_nodelist(net_node, nAttr)
NodeList <- na.omit(NodeList)
class(NodeList)

print(NodeList)
print(EdgeList)

# turn it into a network
eurovisionnet <- igraph::graph_from_data_frame(EdgeList, NodeList, directed = TRUE)

snafun::extract_vertex_attribute(eurovisionnet)
snafun::extract_edge_attribute(eurovisionnet)

## GRAPH 2

# Calculate in-degrees
in_degrees <- igraph::degree(eurovisionnet, mode = "in")

vertex_size <- in_degrees * 1.2  
# Plot the graph with scaled vertex size

plot(
  eurovisionnet,
  edge.arrow.size = 0.2,
  edge.color = "gray80",
  vertex.frame.color = "#ffffff",
  vertex.label.cex = 0.6,
  vertex.label.color = "black",
  vertex.size = vertex_size
)  

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
