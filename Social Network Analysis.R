#########################################################################################
############################### Social Network Analysis #################################
#########################################################################################


##############################
######## load packages #######
##############################
# if the packages below are not installed, then uncomment the install.packages() lines and run them
#install.packages("dplyr")
#install.packages("igraph")
library(dplyr) # dplyr package is used for data manipulation; it uses pipes: %>%
library(igraph) # used to do social network analysis
library(ggplot2)
library(RColorBrewer)

##############################
##### read the data in R #####
##############################
# it's good practice to set up the working directory
# all the files youo read in R or write from R will be in the working directory you set up
# if you copy the path of your file from the foler, change all the \ to /
setwd("/Users/yao/Quarter 1/MIS/Project-Herry Potter")
scripts <- read.csv("1-144.csv")

# keep only the first 2 columns to which we will apply Social Network Analysis
scripts <- scripts %>% select(Character1, Character2)
# there in the Character 2 column, there are some instances in which the character name is not populated
# remove these instances
scripts <- scripts %>% filter(Character2 != "")

# there are 58 different characters in the first column
length(unique(scripts$Character1))
unique(scripts$Character1)
# there are 56 different characters in the second column
length(unique(scripts$Character2))
unique(scripts$Character2)
  

# keep only the first 2 columns to which we will apply Social Network Analysis
scripts <- scripts %>% select(Character1, Character2)

# in social network analysis, we need 2 types of files:
    # conversations: a data frame that shows who talks to whom and how many times
    # nodes: a vector that stores all the character names

# in social network analysis, we shouldn't have duplicate rows, while keeping track of how many times a character talks to another
# in the counts column, write how many times characters communicate with each other
conversations <- scripts %>% group_by(Character1, Character2) %>% summarise(counts = n())

# keep just 50 random conversations in this notebook
set.seed(42) # setting a seed allows us to select the same sample every time
conversations <- conversations[sample(nrow(conversations), 50), ]


# store the character names in a vector
nodes <- c(as.character(conversations$Character1), as.character(conversations$Character2))
nodes <- unique(nodes)


# create the igraph object
# the graph_from_data_frame() function takes in 2 objects: 
# edges: who talks to whom
# nodes (vertices): the unique list of all the characters included in the conversations 
my_graph <- graph_from_data_frame(d=conversations, vertices=nodes, directed=FALSE)
my_graph # 36 nodes & 50 edges


# view the names of each node
V(my_graph)$name

# view the edges 
E(my_graph)

# plot the graph (click on Zoom to see it larger)
plot(my_graph, vertex.color = '#92C5DE',vertex.label.color = "black",vertex.label.cex = .8,vertex.size = 20)

# try different layouts of plotting the graph
# circle layout
plot(my_graph, vertex.label.color = "black", vertex.color = '#92C5DE',vertex.size = 20, vertex.label.cex = .7 ,layout = layout_in_circle(my_graph))
# Fruchterman-Reingold layout 
plot(my_graph, vertex.label.color = "black", vertex.color = '#92C5DE',layout = layout_with_fr(my_graph))
# tree layout 
plot(my_graph, vertex.label.color = "black", vertex.color = '#92C5DE',vertex.label.cex = .7,layout = layout_as_tree(my_graph))

# Create a vector of weights based on the number of conversations each pair has
w1 <- E(my_graph)$counts


# plot the network varying edges by weights
# the thicker the width of the edge, the more conversations that pair has
plot(my_graph, 
     vertex.label.color = "black", 
     edge.color = "gray33",
     vertex.color = '#92C5DE',
     vertex.label.cex = .9,
     edge.width = sqrt(w1),  # put w1 in sqrt() so that the lines don't become too wide
     layout = layout_nicely(my_graph))



# create a new igraph object by keeping just the pairs that have at least 2 conversations 
my_graph_2more_conv <- delete_edges(my_graph, E(my_graph)[counts < 2])

# plot the new graph 
plot(my_graph_2more_conv, 
     vertex.label.color = "black", 
     edge.color = 'gray33',
     vertex.color = '#92C5DE',
     edge.width = sqrt(E(my_graph_2more_conv)$counts),
     layout = layout_nicely(my_graph_2more_conv))


# up until this point, we have only displayed undirected graphs
# therefore, the direction of the conversation was not accounted for

# create a new graph that takes into consideration the direction of the conversation
g <- graph_from_data_frame(conversations, directed = TRUE)
g
# Is the graph directed?
is.directed(g)

# plot the directed network; notice the direction of the arrows, they show the direction of the conversation
plot(g, 
     vertex.label.color = "black", 
     edge.color = 'gray33',
     vertex.size = 0,
     edge.arrow.size = 0.03,
     layout = layout_nicely(g))


# identify all neighbors of 'Harry' regardless of direction
neighbors(g, 'HARRY', mode = c('all'))

# identify the nodes that go towards 'Harry'
neighbors(g, 'HARRY', mode = c('in'))

# identify the nodes that go from 'Harry'
neighbors(g, 'HARRY', mode = c('out'))


# identify any vertices that receive an edge from 'Harry' and direct an edge to 'Hagrid'
n1 <- neighbors(g, 'HARRY', mode = c('out'))
n2 <- neighbors(g, 'HAGRID', mode = c('in'))
intersection(n1, n2)


# determine which 2 vertices are the furthest apart in the graph
farthest_vertices(g) 
# shows the path sequence between two furthest apart vertices
get_diameter(g)  


# identify vertices that are reachable within two connections from 'Harry'
ego(g, 2, 'SNAPE', mode = c('out'))

# identify vertices that can reach Harry' within two connections
ego(g, 2, 'SNAPE', mode = c('in'))


# calculate the out-degree of each vertex
# out-degree represents the number of vertices that are leaving from a particular node
g.outd <- degree(g, mode = c("out"))
g.outd

# find the vertex that has the maximum out-degree
which.max(g.outd)

# calculate betweenness of each vertex
# betweeness is an index of how frequently the vertex lies on shortest paths between any two vertices 
  # in the network. It can be thought of as how critical the vertex is to the flow of information 
  # through a network. Individuals with high betweenness are key bridges between different parts of 
  # a network.
g.b <- betweenness(g, directed = TRUE)
g.b

# Remove multiple or loop edge
g2 <-simplify(g,remove.multiple = TRUE, remove.loops = TRUE)

# Set contributes of nodes
deg <- degree(g2, mode="all")
V(g2)$label <- V(g2)$name
V(g2)$label<-V(g2)$name
V(g2)$degree <- degree(g2)
V(g2)$size <- V(g2)$name
V(g2)$label.cex <- seq(0.5,5,length.out=0.5)
V(g2)$label.size <-(degree(g2)+30)/max(degree(g2))*20

# Creat a Girvan-Newman model and visualize communities

ceb <- cluster_edge_betweenness(g2)
dendPlot(ceb) 
plot(ceb,g2,edge.arrow.size = 0.04,vertex.label.family = "Helvetica",
     vertex.label.color = 'gray11',
     edge.color = 'gray33',
     vertex.size=deg*1.8,
     vertex.label.cex= deg*0.2,
     edge.curved = FALSE,
     layout=layout_with_fr)

save.image("/Users/yao/Quarter 1/MIS/Project-Herry Potter/Socialnetwork_cluster.RData")
