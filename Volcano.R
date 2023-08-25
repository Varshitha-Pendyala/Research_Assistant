

library(readr)
sixtyoneproteins <- read_csv("C:/Users/kvanarsa/OneDrive - University Of Houston/James Di/sixtyoneproteins.csv")
View(sixtyoneproteins)


library(ggplot2)
library(ggplot.multistats)



color_key<-sixtyoneproteins$FC*0
color_key[sixtyoneproteins$FC>2 & sixtyoneproteins$Utestp>0.05] = "FC>2,p>0.05"
color_key[sixtyoneproteins$FC>2 & sixtyoneproteins$Utestp<0.05] = "FC>2,p<0.05"
color_key[sixtyoneproteins$FC>5 & sixtyoneproteins$Utestp<0.001] = "FC>5,p<0.05"
color_key[sixtyoneproteins$FC<=2 & sixtyoneproteins$Utestp>0.05] = "FC<2,p<0.05"

p <- ggplot(sixtyoneproteins,aes(x=log(sixtyoneproteins$FC,base=2),
                                  y=-log(sixtyoneproteins$Utestp,base=10)),
            xlim=c(0,400),ylim=c(0,1)) +geom_point(aes(colour= color_key),size = 1)+
  scale_color_manual(values=c("FC>2,p<0.05"="blue","FC>2,p>0.05"="green","FC>5,p<0.05"="brown","FC<2,p<0.05"="red")) + 
  geom_hline(yintercept = -log10(0.05), linetype = 2, alpha = 0.5, color='black') +
  geom_vline(xintercept = log2(2), linetype = 2, alpha = 0.5, color='black') 

  #geom_hline(yintercept = -log10(0.05)) +   geom_hline(yintercept = -log10(0.001))




p  

update_labels(p + theme_bw(),list(x="log2foldchange", y="-log10pvalue", colour = "volcano plot"))

