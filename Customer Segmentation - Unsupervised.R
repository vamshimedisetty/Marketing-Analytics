# Segmentation by using Clustering  - Hierarchichal and K-Means

## Clusterinhg Techniques

# We wish to examine the clustering methods for intrinsic groupings (Unsupervised Learning)

# Let us load the data
# We have loaded the raw data, removed the dependent varaible - So that the clustering techniques can be applied.

seg.raw <- read.csv('C:/Users/vamsh/Desktop/Marketing Analytics/R_for_market_research/Data_Files/rintro-chapter5.csv')
seg.df = seg.raw[,-7]
View(seg.df)
summary(seg.df)

# Let us create a customized function - to summarize and quickly inspect groups' key characteristcs
    # This will help us understand how groups are different

seg.summ = function(data,groups) {
  aggregate(data,list(groups), function(x) mean(as.numeric(x)))
}

seg.summ(seg.raw[,-7], seg.raw$Segment) ## Running the function on existing data
       
        # This gives us a idea of how each segment is different w.r.t to the independent variables.


# Hierarchical Clustering
# As part of hierachical clustering - we need to measure distance first

d = dist(seg.df[,c("age","income","kids")]) # taking only the continuous variables
as.matrix(d)[1:5, 1:5] # Examining the first five rows


library(cluster)
seg.dist = daisy(seg.df) # Daisy works with mixed datatypes and also 
as.matrix(seg.dist)[1:5,1:5]

seg.hc = hclust(seg.dist, method = "complete")
plot(seg.hc) # Plotting the cluster dendogram 
             # The way to interpret this dendogram is by height. 
             # Draw a horizontal line - The number of intersections made is the number of clusters formed

plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[1]]) # Selecting a subset of dendogram

seg.df[c(128,137),] # Examining two records which are very closely spaced in dendogram
seg.df[c(101,107),] # Examining two records which are very closely spaced in dendogram
seg.df[c(128,288),] # Examining two records which are distant in dendogram

plot(seg.hc)
rect.hclust(seg.hc, k=4, border = 'tomato3') # Splitting ito 4 clusters (groups)
seg.hc.segment = cutree(seg.hc, k=4)
table(seg.hc.segment) 
seg.summ(seg.df,seg.hc.segment) # Calling the custom summary function defined above
                                # We see that clusters have obvious differences (Segment 1,2 differ by gender)
                                # Clusters 1,2 are different from 3,4 by subscription status
                                # This doesn't have much business value

   # Plot to check if gender is differentiating the clusters
   # The plot clearly confirms that gender is differentiating Clusters 1 and 2
   # and 1,2 and 3,4 clusters are differentiated by Subscription status
   # this is kind of obvious. Often clustering methodologies gives us obvious results
   # hence it is advisable to try out various methods, only then we'll better our results and reap business value

plot(jitter(as.numeric(as.numeric(seg.df$gender))) ~ jitter(as.numeric(seg.df$subscribe)),
     col = seg.hc.segment, yaxt = "n", xaxt = "n", ylab = "", xlab = "")
axis(1, at=c(1,2), labels = c("Subscribe: No","Subscribe: Yes"))
axis(2, at=c(1,2), labels = levels(seg.df$gender))


 # Mean based clustering - K Means Clustering

 seg.df.num = seg.df
 seg.df.num$gender = ifelse(seg.df$gender=="Male",0,1)
 seg.df.num$ownHome = ifelse(seg.df$ownHome=="ownNo",0,1)
 seg.df.num$subscribe = ifelse(seg.df$subscribe=="subNo",0,1) # Converting everything into numeric

 seg.df.num = seg.df.num [,-6] # Removing the independent variable
 summary(seg.df.num)
 View(seg.df.num)
 
 set.seed(96743) # this is needed while doing K-means
                 # K-means clustering also forces the analyst to specify the number of clusters
                 # hence this is trial and error approach
 
 # No. of clusters = 4
 
 seg.k = kmeans(seg.df.num, centers = 4)
 summary(seg.k)
 seg.summ(seg.df,seg.k$cluster) # We observe there are interesting differences unlike hclust results
 
 boxplot(seg.df.num$income ~ seg.k$cluster, ylab="income",xlab = "cluster") 
                   # We see income varies across all the clusters formed
 
 library(cluster)
 
 clusplot(seg.df,seg.k$cluster,color = TRUE,shade = TRUE, 
          labels = 4, lines = 0, main = "K-means cluster plot") # Plotting clusters 
 
 
 # No. of clusters = 3
 
 seg.k = kmeans(seg.df.num, centers = 3)
 summary(seg.k)
 seg.summ(seg.df,seg.k$cluster) # We observe there are interesting differences unlike hclust results
 
 boxplot(seg.df.num$income ~ seg.k$cluster, ylab="income",xlab = "cluster") 
 # We see income varies across all the clusters formed
 
 library(cluster)
 
 clusplot(seg.df,seg.k$cluster,color = TRUE,shade = TRUE, 
          labels = 3, lines = 0, main = "K-means cluster plot") # Plotting clusters 
 
     # We obserbve that one of the key drawback of this apprach is that the analyst has to 
     # specify the number of clusters, hence let's examine a few more clustering techniques
 
 
 
 
 
 
 

 
 
 



    





















