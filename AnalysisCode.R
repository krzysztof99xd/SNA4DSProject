library(readxl)
library(tinytex)

## IMPORTING DATA PART

# Set the working directory to the directory where the project is located
setwd("your_path_to_project")
print(getwd())

# Create a relative path to the "data" folder in your project directory
data_folder <- file.path("data")
print(data_folder)

# Extract the absolute path from the relative path
absolute_path_to_data_folder <- normalizePath(data_folder)

file_name <- 'PublicEurovisionBipartite.xlsx'
file_name_node_list <- 'Node_List_Eurovision.xlsx'

# Combine the data folder path and file name to create the full path
full_path_to_node_list <- file.path(data_folder, file_name_node_list)

## read data from csv/excel file
eurovision_public_data_node_list <- read_excel(full_path_to_node_list)

(NodeList_euro <- unique(c(eurovision_public_data_node_list$Node_country)))
NodeList_euro <- na.omit(NodeList_euro) # always remove NAs
class(NodeList_euro)
print(eurovision_public_data_node_list)

# Combine the data folder path and file name to create the full path
full_path_to_bipartite <- file.path(data_folder, file_name)

incedence_df <- read_excel(full_path_to_bipartite)

print(incedence_df)

incedence_df <- as.data.frame(incedence_df)

incedence_matrix <- as.matrix(incedence_df)

print(incedence_matrix)

# Extract receiver names and create a subset without the receiver column
receivers <- incedence_matrix[, 1]

voting_matrix <- incedence_matrix[, -1]  # Remove the first column (receivers)

# Convert string matrix to integers
voting_matrix_int <- matrix(as.integer(as.matrix(voting_matrix)), nrow = nrow(voting_matrix))

distributors <- colnames(voting_matrix)

# Create an empty graph for the Eurovision network
eurovision_graph <- igraph::make_empty_graph(n = length(receivers) + length(distributors))

## CREATE NETWORK 

# Create an incidence matrix with the dimensions of the length of receivers and distributors
inc_matrix <- matrix(0, nrow = length(receivers), ncol = length(distributors))

for (i in 1:nrow(voting_matrix_int)) {
  for (j in 1:ncol(voting_matrix_int)) {
    if (voting_matrix_int[i, j] > 7) {
      inc_matrix[i, j] <- 1
    }
  }
}

# Find the indices of edges based on the incidence matrix
edges <- which(inc_matrix == 1, arr.ind = TRUE)

receiver_indices <- edges[, 1]
distributor_indices <- edges[, 2] + length(receivers)

# Add edges to the graph based on indices
eurovision_graph <- igraph::add_edges(eurovision_graph, cbind(receiver_indices, distributor_indices))

# Create a bipartite graph from the incidence matrix, 
eurovision_graph <- igraph::graph_from_incidence_matrix(inc_matrix)
print(eurovision_graph)

# Project the bipartite graph to get a graph of countries receiving votes
distribution_graph <- igraph::bipartite_projection(eurovision_graph, which = FALSE)

## Now we need to add Node attributes so we must match them with receivers
filtered_node_list <- subset(eurovision_public_data_node_list, Node_country %in% receivers)

# Create an index representing the order of countries in the receivers list, this is very important for maintaining the node attributes for the countries
index_order <- match(filtered_node_list$Node_country, receivers)

# Sort the dataframe based on the index order
sorted_filtered_node_list <- filtered_node_list[order(index_order), ]

nAttrNodeCountry <- sorted_filtered_node_list$Node_country
nAttrCountryPopulation <- sorted_filtered_node_list$country_population
nAttrCountryLangaugeFamily <- sorted_filtered_node_list$country_language_family 
nAttrCountryGovernmentSystem <- sorted_filtered_node_list$country_government_system

# Get country names from the existing graph
existing_countries <- igraph::V(distribution_graph)$Node_country

plot(distribution_graph)

# Add node node attributes 
igraph::V(distribution_graph)$name <- nAttrNodeCountry
igraph::V(distribution_graph)$country_language_family <- nAttrCountryLangaugeFamily
igraph::V(distribution_graph)$country_government_system <- nAttrCountryGovernmentSystem

print(igraph::get.vertex.attribute(distribution_graph))

#REMOVE ISOLATES
isolated_vertices <- which(igraph::degree(distribution_graph, mode = "all") == 0)
distribution_graph_without_isolates <- igraph::delete_vertices(distribution_graph, isolated_vertices)
igraph::V(distribution_graph)$name

#To network
distribution_network_without_isolates <- snafun::to_network(distribution_graph_without_isolates)
print(class(distribution_network_without_isolates))

plot(
  distribution_graph_without_isolates,
  edge.arrow.size = 0.01,
  edge.color = "gray80",
  vertex.frame.color = "#ffffff",
  vertex.label.cex = 0.9,
  vertex.label.color = "black",
  vertex.size = 10
)

## DESCRIPTIVE ANALYSIS PART

snafun::plot_centralities(distribution_graph_without_isolates)

## number of vertices for the network without isolates
snafun::count_vertices(distribution_graph_without_isolates)
## number_of_edges for the network without isolates
snafun::count_edges(distribution_graph_without_isolates)
## density  for the network without isolates
snafun::g_density(distribution_graph_without_isolates)
## reciprocity for the network without isolates
snafun::g_reciprocity(distribution_graph_without_isolates)
## transitivity for the network without isolates
snafun::g_transitivity(distribution_graph_without_isolates)
## mean_distance for the network without isolates
snafun::g_mean_distance(distribution_graph_without_isolates)
## number_of_isolates for the network without isolates
snafun::find_isolates(distribution_graph_without_isolates)
## dyad_census for the network without isolates
snafun::count_dyads(distribution_graph_without_isolates)
## triad_census for the network without isolates
snafun::count_triads(distribution_graph_without_isolates)

distribution_network <- snafun::to_network(distribution_graph)
print(class(distribution_network))
plot(
  distribution_graph,
  edge.arrow.size = 0.01,
  edge.color = "gray80",
  vertex.frame.color = "#ffffff",
  vertex.label.cex = 0.6,
  vertex.label.color = "black",
  vertex.size = 10
)

summary(distribution_network ~ gwesp())
summary(distribution_network ~ degree(0:10))
print(snafun::list_vertex_attributes(distribution_graph))

## number of vertices
snafun::count_vertices(distribution_network)
## number_of_edges
snafun::count_edges(distribution_network)
## density 
snafun::g_density(distribution_network)
## reciprocity
snafun::g_reciprocity(distribution_network)
## transitivity
snafun::g_transitivity(distribution_network)
## mean_distance
snafun::g_mean_distance(distribution_network)
## number_of_isolates
snafun::find_isolates(distribution_network)
## dyad_census
snafun::count_dyads(distribution_network)
## triad_census
snafun::count_triads(distribution_network)

### CUG TEST for community detection
walktrap <- function(x, directed = TRUE) {
  x <- snafun::fix_cug_input(x, directed = directed)
  snafun::extract_comm_walktrap(x) |> length() 
}
## prerequisity->  network parameter must be a network object, not an igraph
distribution_coms <- sna::cug.test(distribution_network, FUN = walktrap, mode = "graph",
                                   diag = FALSE, cmode = "dyad.census", reps = 1000)

print(distribution_coms)

plot(distribution_coms)

#Community visualization 
walktrap_communities <- igraph::cluster_walktrap(distribution_graph)
membership <- igraph::membership(walktrap_communities)
palette <- rainbow(max(membership))
node_colors <- palette[membership]
plot(distribution_graph, vertex.color = node_colors)


## baseline model just with edges 
baseline_model_0.1 <- ergm::ergm(distribution_network ~ edges)
(s1<- summary(baseline_model_0.1)) 

## baseline model with covariate terms
baseline_model_0.2 <- ergm::ergm(distribution_network ~ edges + 
                                   nodematch("country_government_system"))
(s2<- summary(baseline_model_0.2))


## baseline model with covariate terms
baseline_model_0.3 <- ergm::ergm(distribution_network ~ edges + 
                                   nodematch("country_language_family"))
(s3<- summary(baseline_model_0.3))

## baseline model with covariate terms
baseline_model_0.4 <- ergm::ergm(distribution_network ~ edges + 
                                   nodematch("country_government_system") +
                                   nodematch("country_language_family"))
(s4<- summary(baseline_model_0.4))


texreg::screenreg(list(baseline_model_0.1, baseline_model_0.2, baseline_model_0.3, baseline_model_0.4))

## when I set gwesp to false, the R session is aborted :( 
# degree(3) + kstar(2) + gwesp(decay=0.01, fixed= TRUE) never converges
# with decay = 0.0001 the error is 15 so lower 
# with gwdegree(decay = 0.001, fixed = TRUE) never converges
# with degree(0) MCMC still look good but GOF does not look ok for model statistics and still huge standard error :/ 
## Using degree(2:3) does not work :( but edge-wise shared partners looked perfectly
# gwesp(1, fixed=FALSE) did not work at all 
baseline_model_0.5 <- ergm::ergm(distribution_network ~ edges + degree(3) + gwesp(decay = 0.0001, fixed=TRUE) +
                                  nodematch('country_government_system') +
                                  nodematch('country_language_family'),
                                  control = ergm::control.ergm(MCMC.burnin = 10000,
                                                              MCMC.samplesize = 50000,
                                                              seed = 126451,
                                                              MCMLE.maxit = 5,
                                                              parallel = 4,
                                                              parallel.type = "PSOCK"))
## MCMC AND DIAGNOSTICS

(s5 <- summary(baseline_model_0.5))

ergm::mcmc.diagnostics(baseline_model_0.5)


### GOF
baseline_model_0.5_GOF <- ergm::gof(baseline_model_0.5)

snafun::stat_plot_gof(baseline_model_0.5_GOF)

snafun::stat_ef_int(baseline_model_0.5, type = "prob")
