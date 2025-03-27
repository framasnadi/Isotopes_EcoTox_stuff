
library(nicheROVER)
library(readxl)
library(dplyr)
library(ggplot2)

setwd("C:/Users/frma6502/Desktop/SU/Emirdata")
# read in the data
mydata  <- read_excel("Niche_analysis.xlsx") %>% rename( Species ="Species_(BM_or_Mytilopsis)" )
summary(mydata); unique(mydata$Species)
# Summarise By Group (sbg)
sbg <- mydata %>% 
  group_by(Species, LOCATION) %>% 
  summarise(count = n(),
            mC = mean(d13C), 
            sdC = sd(d13C), 
            mN = mean(d15N), 
            sdN = sd(d15N))

# general ggplot showing overlap in niche size between area
jpeg("Biplot_by Area.jpeg",width = 250, height = 200, units = "mm", res = 600)
ggplot(mydata %>% dplyr::filter(LOCATION %in% c(unique(mydata$LOCATION)[5:8])), aes(d13C,d15N))+ theme_bw()  + geom_point(aes(color = Species, shape = Species), size=2.2) +theme(legend.position="left")+theme(legend.position="right") +
  facet_wrap(~ LOCATION  )+ 
  stat_ellipse(aes(group = interaction(Species, LOCATION), 
                   fill = Species, 
                   color = Species), 
               alpha = 0.25, 
               level = 0.95,
               type = "norm",
               geom = "polygon") +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  geom_errorbar(data = sbg%>% dplyr::filter(LOCATION %in% c(unique(mydata$LOCATION)[5:8])), 
                mapping = aes(x = mC, y = mN,
                              ymin = mN - 1.96*sdN, 
                              ymax = mN + 1.96*sdN), 
                width = 0) +
  geom_errorbarh(data = sbg%>% dplyr::filter(LOCATION %in% c(unique(mydata$LOCATION)[5:8])), 
                 mapping = aes(x = mC, y = mN,
                               xmin = mC - 1.96*sdC,
                               xmax = mC + 1.96*sdC),
                 height = 0) + 
  geom_point(data = sbg%>% dplyr::filter(LOCATION %in% c(unique(mydata$LOCATION)[5:8])), aes(x = mC,   y = mN,
                                                                                             fill = Species,shape = Species, color = Species,size=3),
             alpha = 0.9, show.legend = FALSE) 
dev.off()



##################################
# nicheROVER method
#################################
# loop to generate and SAVE in your Working Directory niche overlap plots for 95% niche region sizes + result table
listoverlap <-  list()
for (e  in unique(mydata$LOCATION)[5:8]) {
  mydataoverl <- mydata  %>% dplyr::filter(LOCATION == e ) 
  nsamples <- 1000
  data.par <- tapply(1:nrow(mydataoverl), mydataoverl$Species,
                     function(ii) niw.post(nsamples = nsamples, X = mydataoverl[ii,5:6]))
  over.stat <- overlap(data.par, nreps = nsamples, nprob = 10000, alpha = c(.95))
  over.mean <- round(apply(over.stat, c(1:2), mean)*100,2);over.mean #The mean overlap metrics calculated across iteratations for both niche 
  listoverlap[[e]] <- over.mean
  # Overlap plot
  par(cex.axis = 1, las = 1, mar = c(11, 6, 4, 2) + 0.1)
  clrs <- c( "red", "blue") # colors for each species
  jpeg(paste0("Overlap "  ,e," plot.jpeg"),width = 250, height = 200, units = "mm", res = 600)
  overlap.plot(over.stat, col = clrs, mean.cred.col = "black", equal.axis = T,
               xlab = "Overlap Probability (%) -- Niche Region Size: 95%")
  
  dev.off()
}
# show and save overlap 
listoverlap
write.csv(as.data.frame(listoverlap), "listoverlap.csv")


