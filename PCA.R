# PCA Example 

# Loading:
library(datasets)
library(gplots)
library(ggplot2)
library(scatterplot3d)


library(readr)
JamesdiRFA191 <- read_csv("C:/Users/kvanarsa/OneDrive - University Of Houston/James Di/JamesdiRFA191.csv")
View(JamesdiRFA191)




# Converts data frame to a numeric matrix 
Trial_datamatrix<-data.matrix(JamesdiRFA191[,2:81]) # subtracting first column (the names) so just the data
View(Trial_datamatrix)
#typeof(Trial_datamatrix)


# Converts data.table into a matrix, optionally using one of the columns in the 
# data table as the matrix row names --> The protein names (all rows, 1st columns)
row.names(Trial_datamatrix) <- as.matrix(JamesdiRFA191[,1]) 
View(row.names(Trial_datamatrix))

# Now we call prcomp() to do PCA on our data 
#   - The goal is to draw a graph to show how the samples are related (or not related) to each other 
#   - Note: by default, prcomp() expects the samples to be rows and the genes to be columns 
#           - Since the samples in the data matrix are columns and the proteins are rows, we have to transponse the matrix using the t() function
#           - If you don't transpose the matrix, will just get a graph that shows how the genes are related to each other
pca <- prcomp(t(Trial_datamatrix), scale = TRUE)


# prcomp() returns three things 
#   - x
#   - sdev
#   - rotation



# To get a sense of how meaningful these clusters are, let's see how much variation 
# in the original data PC1 accounts for 
# To do this, using the square of sdev, (standard deviation), to calculate how much variation 
# in the original data each principal component accounts for 
pca.var <- pca$sdev^2



# Since the percentage of variation that each PC accounts for is way more interesting 
# than the actual value, we calculate the percentages:
pca.var.per <- round(pca.var/sum(pca.var)*100,1)

# Uncomment to save image
# Adjust parameters as needed to best fit the figure 
#jpeg(file="PCA.jpeg",
#     height = 10, width = 20, units='cm', res = 900)

# Now plotting the percentages with barplot()
barplot(pca.var.per, main = "CasesvsControl PCA", xlab = "Principal Component", ylab = "Percent Variation",names.arg = c("1","2",
                                                                                                                     "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))


# Now we can use ggplot2 to make a PCA plot that looks better and provides more information
pca.data <- data.frame(Sample=rownames(pca$x),X = pca$x[,1],Y = pca$x[,2])
pca.data
# data frame has one row per sample ID, and X/Y coordinates for that sample



# Add extra space to right of plot area; change clipping to figure
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)



# pch = c(rep(16 #this means filled circle, 11 #this means how man to fill in))
plot(pca$x[,1],pca$x[,2],pch = c(rep(16,10),rep(16,10)),col=c(rep("red",10),rep("green",7)),xlab = "PC 1 - 20.3%",ylab = "PC 2 - 13.2%",main = "CasesvsControl")


# Adding Legend
legend("topright", inset=c(-0.2,0), legend=c("Cases","Control"), pch=c(16,16), col = c("red","green"),title="Groups")



# Adding grid for easier reading
par(xpd=FALSE) # This makes sure grid is confined to plot
grid(NULL,NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

#dev.off()


# ---------------------------------------------------------------------------- #
# Now plotting by UC, CD, and HC - 3 different colors
# Add extra space to right of plot area; change clipping to figure
#par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)


# The following is for Stool: 
#   - The first 5 are CD
#   - The next 5 are UC
#   - The last 7 are HC
# pch = c(rep(16 #this means filled circle, 11 #this means how man to fill in))
#plot(pca$x[,1],pca$x[,2],pch = c(rep(16,5),rep(16,5),rep(16,7)),col=c(rep("red",5),rep("blue",5),rep("green",7)),xlab = "PC 1 - 20.3%",ylab = "PC 2 - 13.2%",main = "PCA Plasma")


# Adding Legend
#legend("topright", inset=c(-0.2,0), legend=c("CD","UC","HC"), pch=c(16,16,16), col = c("red","blue","green"),title="Groups")



# Adding grid for easier reading
#par(xpd=FALSE) # This makes sure grid is confined to plot
#grid(NULL,NULL, col = "lightgray", lty = "dotted",
#     lwd = par("lwd"), equilogs = TRUE)


# ---------------------------------------------------------------------------- #
# 3D PCA plot using scatterplot3d

# Uncomment to save image
#jpeg(file="PCA.jpeg",
#     height = 10, width = 20, units='cm', res = 900)

# Add extra space to right of plot area; change clipping to figure
#par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
par(mar=c(10, 10, 10, 10), xpd=TRUE)


# This is 3D scatterplot Cases vs. Control:
scatterplot3d(pca$x[,1],pca$x[,2],pca$x[,3],pch = c(rep(16,10),rep(10,10)),color=c(rep("red",10),rep("green",10)),xlab = "PC 1 - 20.3%",ylab = "PC 2 - 13.2%", zlab = "PC 3 - 12.1%",main = "CasesvsControls", box = TRUE)

# Adding grid:
#addgrids3d(grid = c("xy","xz","yz"))


# Adding Legend
legend("bottomright", inset=c(0,-0.15), legend=c("Cases","Control"), pch=c(16,16), col = c("red","green"),title="Groups:",xpd = TRUE, horiz = TRUE)

#dev.off()



# This is 3D scatterplot for UC, CD, HC:
#scatterplot3d(pca$x[,1],pca$x[,2],pca$x[,3],pch = c(rep(16,5),rep(16,5),rep(16,7)),color=c(rep("red",5),rep("blue",5),rep("green",7)),xlab = "PC 1 - 20.3%",ylab = "PC 2 - 13.2%",zlab = "PC 3 - 12.1%",main = "PCA Plasma")


# Adding Legend
#legend("bottomright", inset=c(0,-0.15), legend=c("CD","UC","HC"), pch=c(16,16,16), col = c("red","blue","green"),title="Groups: ")


