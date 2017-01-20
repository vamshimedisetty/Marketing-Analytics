# Segmentation by using Clustering  - Mclust and poLCA

# Mclust is model based clustering

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



seg.df.num = seg.df
seg.df.num$gender = ifelse(seg.df$gender=="Male",0,1)
seg.df.num$ownHome = ifelse(seg.df$ownHome=="ownNo",0,1)
seg.df.num$subscribe = ifelse(seg.df$subscribe=="subNo",0,1) # Converting everything into numeric

seg.df.num = seg.df.num [,-6] # Removing the independent variable
summary(seg.df.num)
View(seg.df.num)



library(mclust)
seg.mc = Mclust(seg.df.num)
summary(seg.mc) # We see that 9 clusters are formed. 
                # This actually decides number of clusters on its own.

seg.mc4 = Mclust(seg.df.num, G = 4) # fixing clusters to 4
summary(seg.mc4) # Models with number of clusters fixed.

seg.mc3 = Mclust(seg.df.num, G = 3) # fixing clusters to 3
summary(seg.mc3) # Models with number of clusters fixed.

 # Comapring models with BIC

BIC(seg.mc,seg.mc4,seg.mc3) 
     
    # the lower the BIC, the better the model.
    # So, the one with 3 clusters is a relatively better model.

seg.summ(seg.df, seg.mc3$class) # Better way of clustering - Nothing obvious 

library(cluster)
clusplot(seg.df, seg.mc3$class, color = TRUE, shade = TRUE,
         labels = 4, lines = 0, main = 'Model based Cluster Plot')

## Latent Class Analysis 
   # This works only on catergorical variables

seg.df.cut = seg.df

seg.df.cut$age = factor(ifelse(seg.df$age < median(seg.df$age),1,2))
seg.df.cut$income = factor(ifelse(seg.df$income < median(seg.df$income),1,2))
seg.df.cut$kids = factor(ifelse(seg.df$kids < median(seg.df$kids),1,2))

summary(seg.df.cut)

seg.f = with(seg.df.cut,cbind(age,gender,income,kids,ownHome,subscribe)~1)

install.packages('poLCA')
library(poLCA)

set.seed(02807)
seg.LCA3 = poLCA(seg.f, data = seg.df.cut, nclass = 3) # 3 clusters
seg.LCA4 = poLCA(seg.f, data = seg.df.cut, nclass = 4) # 4 clusters


seg.LCA3$bic  
seg.LCA4$bic # Checking which model is better


seg.summ(seg.df, seg.LCA3$predclass) # No obvious results - good clusters

table(seg.LCA3$predclass)
table(seg.LCA4$predclass)
clusplot(seg.df,seg.LCA3$predclass,color = TRUE,shade = TRUE, labels = 4,lines = 0, main = 'LCA plot (k=3)')
clusplot(seg.df,seg.LCA4$predclass,color = TRUE,shade = TRUE, labels = 4,lines = 0, main = 'LCA plot (k=4)')

# Confusion matrix
library(mclust)

table(seg.raw$Segment, seg.LCA4$predclass)
adjustedRandIndex(seg.raw$Segment, seg.LCA4$predclass) # Tells us this is 35% better than chance alone.





















