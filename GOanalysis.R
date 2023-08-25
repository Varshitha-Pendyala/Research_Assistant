## Gene Ontology - Bladder Cancer Data Analysis

library(ggplot2)
library(readxl)
library(svglite)

# Biobnological Process - Try 

# y-axis - function 
# x-axis Fold Enrinchment 
# Size - Based on Count - Circle 
# Color - -log10(FDR) - Blue to purple to red

library(readxl)
GO_anlayiss_excel <- read_excel("GO anlayiss excel.xlsx")
View(GO_anlayiss_excel)



GO_anlayiss_excel$FDR <- -log10(GO_anlayiss_excel$FDR)
View(GO_anlayiss_excel)
GO_anlayiss_excel <- data.frame(GO_anlayiss_excel)
View(GO_anlayiss_excel)

q <- ggplot(GO_anlayiss_excel, aes(x = reorder(Term, Fold.Enrichment), y =
                                     Fold.Enrichment)) + ylab("Fold Enrichment") + coord_flip() + geom_point(
                                       aes(size = Count, color = FDR)) + ggtitle("GO - Biological Process")
q

#jpeg(file="GO_anlayiss_excel Top102.jpeg",
     height = 10, width = 20, units='cm', res = 900)

q1 <- q + theme_classic() + theme(panel.border = element_rect(color =
                                                                "black",
                                                              fill = NA,
                                                              size = 1)) +
  guides(size = guide_legend(order = 2), col = guide_colorbar(order = 1))
q1

jpeg(file="GO_anlayiss_excel Top103.jpeg",
     height = 10, width = 20, units='cm', res = 900)
q2 <- q1 + scale_color_gradient(low = "blue", high = "red") + labs(size =
                                                                     expression("Count"), col = expression("-log"[10]*"FDR"))
q2
q3 <- q2 + theme(axis.title.y = element_blank())
q3
dev.off()


jpeg(file="GO_anlayiss_excel Top10.jpeg",
     height = 10, width = 20, units='cm', res = 900)



