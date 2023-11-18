sna4tutti::open_sna4tutti_tutorials()

library(readxl)
library(tinytex)

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

## read data from csv file
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
print(receivers)

voting_matrix <- incedence_matrix[, -1]  # Remove the first column (receivers)

# Convert string matrix to integers
voting_matrix_int <- matrix(as.integer(as.matrix(voting_matrix)), nrow = nrow(voting_matrix))

print(voting_matrix_int)

distributors <- colnames(voting_matrix)

print(distributors)
print(receivers)

# Create an empty graph for the Eurovision network
eurovision_graph <- igraph::make_empty_graph(n = length(receivers) + length(distributors))

# Create an incidence matrix
inc_matrix <- matrix(0, nrow = length(receivers), ncol = length(distributors))

for (i in 1:nrow(voting_matrix_int)) {
  for (j in 1:ncol(voting_matrix_int)) {
    if (voting_matrix_int[i, j] > 7) {
      inc_matrix[i, j] <- 1
    }
  }
}
print(inc_matrix)

# Find the indices of edges based on the incidence matrix
edges <- which(inc_matrix == 1, arr.ind = TRUE)

receiver_indices <- edges[, 1]
distributor_indices <- edges[, 2] + length(receivers)

# Add edges to the graph based on indices
eurovision_graph <- igraph::add_edges(eurovision_graph, cbind(receiver_indices, distributor_indices))

# Create a bipartite graph from the incidence matrix
eurovision_graph <- igraph::graph_from_incidence_matrix(inc_matrix)
print(eurovision_graph)

# Project the bipartite graph to get a graph of countries distributing votes
distribution_graph <- igraph::bipartite_projection(eurovision_graph, which = FALSE)
print(distribution_graph)

filtered_node_list <- subset(eurovision_public_data_node_list, Node_country %in% receivers)

# Create an index representing the order of countries in the receivers list, this is very important for maintaing the node attributes for the countries
index_order <- match(filtered_node_list$Node_country, receivers)

# Sort the dataframe based on the index order
sorted_filtered_node_list <- filtered_node_list[order(index_order), ]

nAttr0 <- filtered_node_list$Node_country
nAttr <- filtered_node_list$country_population
nAttr2 <- filtered_node_list$country_language_family 
nAttr3 <- filtered_node_list$country_government_system

# Get country names from the existing graph
existing_countries <- igraph::V(distribution_graph)$name
print(existing_countries)

plot(distribution_graph)

# Add node names for receivers 
igraph::V(distribution_graph)$name <- nAttr0
igraph::V(distribution_graph)$country_language_family <- nAttr2
igraph::V(distribution_graph)$country_government_system <- nAttr3


print(igraph::get.vertex.attribute(distribution_graph))

# Check if the length of nAttr matches the number of nodes in the graph
if (length(nAttr) == igraph::vcount(distribution_graph)) {
  # Set node attributes for the graph
  igraph::V(distribution_graph)$country_population <- nAttr
} else {
  print("Length of nAttr does not match the number of nodes in the graph.")
}

# Check if the length of nAttr matches the number of nodes in the graph
if (length(nAttr) == igraph::vcount(eurovisionnet)) {
  # Set node attributes for the graph
  igraph::V(distribution_graph)$country_language_family <- nAttr2
} else {
  print("Length of nAttr2 does not match the number of nodes in the graph.")
}

# Check if the length of nAttr matches the number of nodes in the graph
if (length(nAttr) == igraph::vcount(distribution_graph)) {
  # Set node attributes for the graph
  igraph::V(distribution_graph)$country_government_system <- nAttr3
} else {
  print("Length of nAttr3 does not match the number of nodes in the graph.")
}

snafun::plot_centralities(distribution_graph)

summary(distribution_graph ~ degree(0:35))
print(class(distribution_graph))

distribution_network <- snafun::to_network(distribution_graph)
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

## when I set gwesp to false, the R session is aborted :(, gwesp(decay=0.2, fixed = TRUE, cutoff=29) it never converges :( 
baseline_model_0.5 <- ergm::ergm(distribution_network ~ edges + 
                                   gwesp(decay=0.2, fixed = TRUE) +
                                   kstar(3) + 
                                   nodematch("country_language_family") + 
                                   nodematch("country_government_system"),
                                 control = ergm::control.ergm(MCMC.burnin = 5000,
                                                              MCMC.samplesize = 15000,
                                                              seed = 123451,
                                                              MCMLE.maxit = 5,
                                                              parallel = 3,
                                                              parallel.type = "PSOCK"))

(s5 <- summary(baseline_model_0.5))

ergm::mcmc.diagnostics(baseline_model_0.5)


### that doesnt look that good :/ 
baseline_model_0.5_GOF <- ergm::gof(baseline_model_0.5)

snafun::stat_plot_gof(baseline_model_0.5_GOF)

























## Similarly, in your case with Eurovision voting, we might have a bipartite network of countries giving votes and countries receiving votes. 
## If our project it onto one set (say, countries giving votes), you'd create a new network among those countries based on their shared connections (i.e., if they have voted for the same countries).

# Remove the first column to focus on the scores
scores_df <- incedence_df[, -1]

# Create an empty dataframe
empty_df <- data.frame()

# Loop through each column to extract the top 3 scores and their respective countries
for (col in names(scores_df)) {
  # Get the top 3 rows for each column, sort them in descending order
  top_4 <- head(scores_df[order(-scores_df[, col]), col], 2)
  print(top_4)
  
  # Get the country names for the top 3 scores
  countries <- incedence_df[order(-incedence_df[, col]), ][1:2, 1]
  print(countries)
  print(col)
  
  # Combine country names with top 3 scores
  top_scores_list[[col]] <- data.frame(Sender = col, Receiver = countries, Score = top_4)
  empty_df <- rbind(empty_df, top_scores_list[[col]])
}
print(empty_df)

# Get the top 5 receivers for each sender
top_5 <- aggregate(Receiver ~ Sender, data = empty_df, function(x) head(unique(x), 2))

# Create an empty adjacency matrix
countries <- unique(empty_df$Sender)
adj_matrix <- matrix(0, nrow = length(countries), ncol = length(countries), dimnames = list(countries, countries))

# Loop through countries and check for shared receivers
for (i in 1:length(countries)) {
  for (j in (i+1):length(countries)) {
    shared_receivers <- intersect(top_5[top_5$Sender == countries[i], "Receiver"],
                                  top_5[top_5$Sender == countries[j], "Receiver"])
    if (length(shared_receivers) > 0) {
      adj_matrix[countries[i], countries[j]] <- 1
      adj_matrix[countries[j], countries[i]] <- 1
    }
  }
}

# Print the adjacency matrix
print(adj_matrix)

eurovisionnet <- igraph::graph_from_adjacency_matrix(adj_matrix)

nAttr <- eurovision_public_data_node_list$country_population
nAttr2 <- eurovision_public_data_node_list$country_language_family 
nAttr3 <- eurovision_public_data_node_list$country_government_system

# Check if the length of nAttr matches the number of nodes in the graph
if (length(nAttr) == igraph::vcount(eurovisionnet)) {
  # Set node attributes for the graph
  igraph::V(eurovisionnet)$country_population <- nAttr
} else {
  print("Length of nAttr does not match the number of nodes in the graph.")
}


# Check if the length of nAttr matches the number of nodes in the graph
if (length(nAttr) == igraph::vcount(eurovisionnet)) {
  # Set node attributes for the graph
  igraph::V(eurovisionnet)$country_language_family <- nAttr2
} else {
  print("Length of nAttr2 does not match the number of nodes in the graph.")
}

# Check if the length of nAttr matches the number of nodes in the graph
if (length(nAttr) == igraph::vcount(eurovisionnet)) {
  # Set node attributes for the graph
  igraph::V(eurovisionnet)$country_government_system <- nAttr3
} else {
  print("Length of nAttr3 does not match the number of nodes in the graph.")
}

summary(eurovisionnet)

summary(net_eurovision ~ degree(0:35))

summary(net_eurovision ~ gwesp())

snafun::plot_centralities(eurovisionnet)

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


# turn it into a network (is necessary for walktrap community)
print(class(eurovisionnet))
net_eurovision <- snafun::to_network(eurovisionnet)
print(class(net_eurovision))

plot(net_eurovision)


plot(
  eurovisionnet,
  edge.arrow.size = 0.01,
  edge.color = "gray80",
  vertex.frame.color = "#ffffff",
  vertex.label.cex = 0.6,
  vertex.label.color = "black",
  vertex.size = 5
)


summary(net_eurovision)

## Baseline ERGM model (prerequisities, must be a network object)
baseline_model_0.1 <- ergm::ergm(net_eurovision ~ edges)  

(s1 <- summary(baseline_model_0.1))

## Baseline ERGM model (prerequisities, must be a network object), introducing covariate terms
baseline_model_0.2 <- ergm::ergm(net_eurovision ~ edges + nodematch('country_language_family') + nodematch('country_government_system'))  

(s2 <- summary(baseline_model_0.2))


## model works, good MCMC (except country_language_family) improvement on GOF ,introducing dyadic dependent
baseline_model_0.3 <- ergm::ergm(net_eurovision ~ edges + degree(24:29) + nodematch("country_language_family") + nodematch("country_government_system"),
                                 control = ergm::control.ergm(MCMC.burnin = 5000,
                                                              MCMC.samplesize = 15000,
                                                              seed = 123459,
                                                              MCMLE.maxit = 5,
                                                              parallel = 3,
                                                              parallel.type = "PSOCK"))
(s3 <- summary(baseline_model_0.3))

ergm::mcmc.diagnostics(baseline_model_0.3)

snafun::stat_ef_int(baseline_model_0.3, type = "odds")

snafun::stat_ef_int(baseline_model_0.3, type = "prob")


### that doesnt look that good :/ 
baseline_model_0.3_GOF <- ergm::gof(baseline_model_0.3)

snafun::stat_plot_gof(baseline_model_0.3_GOF)


## kstar(3) and cycle(3) does not work at all :/ 
baseline_model_0.4 <- ergm::ergm(net_eurovision ~ edges + degree(24:29) + nodematch("country_language_family") + nodematch("country_government_system"),
                                 control = ergm::control.ergm(MCMC.burnin = 5000,
                                                              MCMC.samplesize = 15000,
                                                              seed = 123459,
                                                              MCMLE.maxit = 5,
                                                              parallel = 3,
                                                              parallel.type = "PSOCK"))
(s4 <- summary(baseline_model_0.4))

ergm::mcmc.diagnostics(baseline_model_0.4)

snafun::stat_ef_int(baseline_model_0.4, type = "odds")

snafun::stat_ef_int(baseline_model_0.4, type = "prob")


### that doesnt look that good :/ 
baseline_model_0.4_GOF <- ergm::gof(baseline_model_0.4)

snafun::stat_plot_gof(baseline_model_0.4_GOF)

summary(net_eurovision ~ degree(0:30))

## when I set gwesp to false, the R session is aborted :(, gwesp(decay=0.2, fixed = TRUE, cutoff=29) it never converges :( 
baseline_model_0.5 <- ergm::ergm(net_eurovision ~ edges + 
                                   gwesp(decay=0.2, fixed = TRUE, cutoff=29) + 
                                   nodematch("country_language_family") + 
                                   nodematch("country_government_system"),
                                   control = ergm::control.ergm(MCMC.burnin = 5000,
                                                              MCMC.samplesize = 15000,
                                                              seed = 123451,
                                                              MCMLE.maxit = 2,
                                                              parallel = 3,
                                                              parallel.type = "PSOCK"))

(s5 <- summary(baseline_model_0.5))

ergm::mcmc.diagnostics(baseline_model_0.5)


### that doesnt look that good :/ 
baseline_model_0.5_GOF <- ergm::gof(baseline_model_0.5)

snafun::stat_plot_gof(baseline_model_0.5_GOF)

