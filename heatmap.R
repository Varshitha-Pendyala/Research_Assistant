library(readxl)
library(readr)
library(RColorBrewer)
library(gplots)

library(readr)
Jamesdiheatmap <- read_csv("Jamesdiheatmap.csv")
View(Jamesdiheatmap)


values<-data.matrix(Jamesdiheatmap[,-1])
row.names(values)<-as.matrix(Jamesdiheatmap[,1])

#change colors to yellow (for red) and blue (for green)
colors <- colorRampPalette(c("blue","grey27", "yellow"))(n=42)

png(file="Jamesdiheatmap.png",
    height = 15, width = 20, units='cm', res = 900) 



heatmap3::heatmap3(values,scale="row",
          main="191",
          col = colors, trace='none', Colv=NA,cexRow = 0.2, density.info = "none", dendrogram = 'row')

dev.off()

print(Jamesdiheatmap)







