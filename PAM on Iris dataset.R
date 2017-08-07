library(cluster) # For the Gower distance matrix and PAM
library(Rtsne)   # For creating the t-SNE dataset for component reduction and plotting
library(ggplot2) # For plotting the data


#======================================================================================
# Read in the data and format where required
#======================================================================================

# Read in the Iris data
data(iris)



#======================================================================================
# Calculate the Gower distance matrix
#======================================================================================

# The "Daisy" function calculates the Gower distance between each pair of data points
# It automatically takes into consideration numeric and categorical columns 
# For Interval (I) i.e Numeric columns  --> Uses Manhattan distance
# For Nominal (N) or Ordinal (O) columns --> Uses the Dice coefficient

# Calculate the Gower distance for all columns except the "Species"
# This will create a "list" which is around N x N elements (N=150 in our data) 
gower_dist = daisy(iris[,-5],metric="gower")

# Checking the summary
summary(gower_dist)

# Now convert the distance list into a matrix
# This would create a 150x150 mattrix --> the distance between each pair of data points
gower_mat = as.matrix(gower_dist)



#======================================================================================
# How many clusters to create?
#======================================================================================

# We will iterate through 2-10 clusters and see which shows the maximum separation between centres
# The "Silhoutte Width" parameter is used by default for PAM clusters
sil_width = c(NA)

for(i in 2:10)
{
  pam_fit = pam(gower_dist, diss = TRUE, k=i)
  sil_width[i] = pam_fit$silinfo$avg.width
}


# Plot the silhoutte widths and see how many clusters is optimal (maximum value of sil_width)
plot(1:10,sil_width,xlab="Number of Clusters",ylab="Silhouette width")
lines(1:10, sil_width)


#======================================================================================
# Create the clusters using PAM
#======================================================================================

# From the graph we see that 2 clusters is optimal, but since we have three species
# so we decide to go with 3 clusters (feel free to try out with 2 though)

pam_fit = pam(gower_dist, diss=TRUE, k=3)


#======================================================================================
# Plot the points in each cluster to see how effective the clustering is
#======================================================================================

# Add the "cluster number" column to our data
iris$cluster = pam_fit$clustering

# t-SNE is a demension reduction technique which will reduce our data to a X:Y format 
# for plotting as a 2D plot
tsne_obj = Rtsne(gower_dist,is_distance = TRUE)


# Create a new dataset for plotting the clusters
# the "Y" parameter of the tsne_obj has the X:Y values 
iris_plot = data.frame(tsne_obj$Y)

# Name the columns as X and Y
names(iris_plot) = c("X","Y")

# Add the clusters identified for each row as a column
# the as.factor() is used to allow R to use it's own color scheme while plotting the points
# Without it it would choose the color as per the cluster number (e.g. 1=black, 2=blue etc.)
iris_plot$cluster = as.factor(iris$cluster)

# Plot the clusters
ggplot(aes(x=X, y=Y), data=iris_plot) + geom_point(aes(color=cluster))





# We can see that PAM does a wonderful job of segregating the iris data into three
# distinct clusters. 
# Hope you enjoyed this tutorial :)
