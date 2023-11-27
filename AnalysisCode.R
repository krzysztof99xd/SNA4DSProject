library(readxl)
library(tinytex)
library(igraph)


# Set the working directory to the directory where the project is located
setwd("C:/Users/thier/Downloads/SNA4DSProject-master/SNA4DSProject-master")
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
voting_matrix <- incedence_matrix[, -1]  # Remove the first column (receivers)

# Convert string matrix to integers
voting_matrix_int <- matrix(as.integer(as.matrix(voting_matrix)), nrow = nrow(voting_matrix))
distributors <- colnames(voting_matrix)

# Create an empty graph for the Eurovision network
eurovision_graph <- igraph::make_empty_graph(n = length(receivers) + length(distributors))

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

# Create a bipartite graph from the incidence matrix, ## In our case with Eurovision voting, we have a bipartite network of countries giving votes and countries receiving votes. 
## If our project it onto one set (say, countries giving votes), you'd create a new network among those countries based on their shared connections (i.e., if they have received votes from the same countries).
eurovision_graph <- igraph::graph_from_incidence_matrix(inc_matrix)
print(eurovision_graph)

# Project the bipartite graph to get a graph of countries distributing votes
distribution_graph <- igraph::bipartite_projection(eurovision_graph, which = FALSE)

#CHANGE THIS TO NODELIST_EURO?? BUT ENSURE NODELIST EURO STILL HAS COLUMN NODE_COUNTRY
## Now we need to add Node attributes so we must match them with receivers
filtered_node_list <- subset(eurovision_public_data_node_list, Node_country %in% receivers)

# Create an index representing the order of countries in the receivers list, this is very important for maintaing the node attributes for the countries
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


# Add node names for receivers 
igraph::V(distribution_graph)$name <- nAttrNodeCountry
igraph::V(distribution_graph)$country_language_family <- nAttrCountryLangaugeFamily
igraph::V(distribution_graph)$country_government_system <- nAttrCountryGovernmentSystem

print(igraph::get.vertex.attribute(distribution_graph))

snafun::plot_centralities(distribution_graph)

#REMOVE ISOLATES
isolated_vertices <- which(degree(distribution_graph, mode = "all") == 0)
distribution_graph <- igraph::delete_vertices(distribution_graph, isolated_vertices)
V(distribution_graph)$name

#To network
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

## baseline model just with edges 
baseline_model_0.1 <- ergm::ergm(distribution_network ~ edges)
(s1<- summary(baseline_model_0.1)) 

## baseline model with covariate terms
baseline_model_0.2 <- ergm::ergm(distribution_network ~ edges + 
                                   nodematch("country_language_family") + 
                                   nodematch("country_government_system"))
(s2<- summary(baseline_model_0.2))



## when I set gwesp to false, the R session is aborted :( 
## gwesp(decay=0.02, fixed=TRUE) + degree(3) works relatively well, lets try to extend from here but lets not limit ourselves to that 
# kstar(1) did not really work :/
# Warning: Model statistics ‘kstar2’ and ‘nodematch.country_language_family’ are linear combinations of some set of preceding statistics at the current stage of the estimation. This may indicate that the model is nonidentifiable
# degree(3) + kstar(2) + gwesp(decay=0.01, fixed= TRUE) never converges
baseline_model_0.5 <- ergm::ergm(distribution_network ~ edges + degree(3)   + gwesp(decay = 0.25, fixed = TRUE) +
                                   nodematch("country_language_family") +
                                   nodematch("country_government_system"),
                                   control = ergm::control.ergm(MCMC.burnin = 1000,
                                                              MCMC.samplesize = 7000,
                                                              seed = 14254,
                                                              MCMLE.maxit = 10,
                                                              parallel = 8,
                                                              parallel.type = "PSOCK"))


ergm::mcmc.diagnostics(baseline_model_0.5)

(s5 <- summary(baseline_model_0.5))

#GOF Geodesic distance probably indicates some issues with regards to isolates. 
baseline_model_0.5_GOF <- ergm::gof(baseline_model_0.5)

snafun::stat_plot_gof(baseline_model_0.5_GOF)
snafun::stat_ef_int(baseline_model_0.5, type = "prob")
snafun::stat_ef_int(baseline_model_0.5, type = "odds")


#CUG test for detecting communities. Better significance using directed = TRUE. Why?
walktrap <- function(x, directed = FALSE) {
  x <- snafun::fix_cug_input(x, directed = directed)
  snafun::extract_comm_walktrap(x) |> length() 
}
distribution_coms <- sna::cug.test(distribution_network, FUN = walktrap, mode = "graph",
                                   diag = FALSE, cmode = "dyad.census", reps = 5000)
print(distribution_coms)
print(distribution_coms$repstats)

    



#Improved Plot of CUG Test.
plot_cug_test <- function(distribution_coms) {
  # Extract data to plot
  data_to_plot <- distribution_coms$rep.stat
  bar_values <- table(data_to_plot)
  
  # Create a bar plot
  cugplot <- barplot(bar_values,
                    main = "CUG Test for Detecting Communities in Eurovision 2023",
                    xlab = "Nr of Communities",
                    ylab = "Frequency",
                    col = "skyblue",
                    space = 0)  
  
  # Calculate position of observed value with respect to the bars
  obs_position <- cugplot[as.numeric(names(bar_values)) == distribution_coms$obs.stat]
  
  # Add baseline to plot
  #tick_positions <- axis(side = 1, at = cugplot, labels = FALSE)  # Get tick positions on the x-axis
  abline(h = 0)  # You can adjust the color and line type as needed
  
  # Add vertical line for observed value
  abline(v = obs_position, col = "red", lwd = 2)
  legend("topright", legend = c("Observed Value"), col = "red", lwd = 2)
  
  # Include conditioning and reps info
  extra_description <- paste(
                             "Conditioning:", distribution_coms$cmode,
                             "Reps:", distribution_coms$reps)
  mtext(text = extra_description, side = 3, line = 0.5, cex = 0.8, col = "black")
}

plot_cug_test(distribution_coms)




#Original plot of CUG Test
plot(distribution_coms,
     main = "CUG Test for Detecting Communities in Eurovision 2023")




#Plot a communities graph 
plot_communities <- function(graph, layout = igraph::layout_with_kk) {
  walktrap_communities <- snafun::extract_comm_walktrap(graph)
  membership <- igraph::membership(walktrap_communities)
  palette <- rainbow(max(membership))
  node_colors <- palette[membership]
  plot(distribution_graph, 
       vertex.color = node_colors, 
       layout = layout,
       main = "Projected Eurovision 2023 Network with Walktrap Communities",   
       vertex.label.dist = 2.2,  
  )
  legend("topright", 
         legend = 1:max(membership), 
         fill = palette, 
         title = "Community")
  
  num_communities <- length(unique(membership))
  cat("Number of communities/clusters:", num_communities, "\n")
}

plot_communities(distribution_graph)
