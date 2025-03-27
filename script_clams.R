rm(list = ls()) # clear the memory of objects

# load the siar package of functions
library(SIBER)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
setwd("C:/Users/frma6502/Desktop/SU/Emirdata")
# read in the data
mydata  <- read_excel("Niche_analysis.xlsx")
summary(mydata); unique(mydata$species)

# sample size 
mydata %>% group_by(LOCATION, Species) %>%  dplyr::count(Species)

# Summarise By Group (sbg)
sbg <- mydata %>% 
  group_by(Species, LOCATION) %>% 
  summarise(count = n(),
            mC = mean(d13C), 
            sdC = sd(d13C), 
            mN = mean(d15N), 
            sdN = sd(d15N))

# biplot
jpeg("Biplot_bulk.jpeg",width = 300, height = 250, units = "mm", res = 600)
ggplot(mydata, aes(d13C,d15N))+ theme_bw()  + geom_point(aes(color = LOCATION, shape = Species), size=2.2) +theme(legend.position="left")+theme(legend.position="right")  + 
  stat_ellipse(aes(group = interaction(Species, LOCATION), 
                   fill = LOCATION, 
                   color = LOCATION), 
               alpha = 0.25, 
               level = 0.95,
               type = "norm",
               geom = "polygon") +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  geom_errorbar(data = sbg, 
                mapping = aes(x = mC, y = mN,
                              ymin = mN - 1.96*sdN, 
                              ymax = mN + 1.96*sdN), 
                width = 0) +
  geom_errorbarh(data = sbg, 
                 mapping = aes(x = mC, y = mN,
                               xmin = mC - 1.96*sdC,
                               xmax = mC + 1.96*sdC),
                 height = 0) + 
  geom_point(data = sbg, aes(x = mC, 
                             y = mN,
                             fill = LOCATION,shape = Species, color = LOCATION,size=3),
             alpha = 0.9, show.legend = FALSE) 
dev.off()



# create the siber object
mydatasb  <- as.data.frame( mydata  %>%  dplyr::rename(iso2 = d15N  , iso1 = d13C, group = Species ,community= LOCATION) %>% dplyr::select(iso1,iso2, group, community))
siber.example <- createSiberObject(mydatasb)
siber.example$sample.sizes
# Calculate sumamry statistics for each group: TA, SEA and SEAc
#group.ML <- as.data.frame(groupMetricsML(siber.example))
#write.csv(group.ML, "species.SEA.ML.csv")

######################################################################
## Using Bayesian Inference to calculate uncertainty around ellipses
######################################################################
# options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 5    # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the 
# means. Fitting is via the JAGS method.
ellipses.posterior <- siberMVN(siber.example, parms, priors)
# extract the posterior means
mu.post <- extractPosteriorMeans(siber.example, ellipses.posterior)


# ----------------------------------------------------------------
# Plot out some of the data and results
# ----------------------------------------------------------------

# The posterior estimates of the ellipses for each group can be used to
# calculate the SEA.B for each group.
SEA.B <- siberEllipses(ellipses.posterior)

jpeg("Niche_SEA.jpeg",width = 520, height = 250, units = "mm", res = 600)
# my_clrs <- matrix(c("tomato", "tomato2", "tomato3","tomato", "tomato2", "tomato3",       "turquoise", "turquoise3", "turquoise4",  "turquoise", "turquoise3", "turquoise4" ),nrow=3,ncol=12)
par(cex.axis = 1, las = 2, mar = c(11, 4, 4, 2) + 0.1)
siberDensityPlot(SEA.B, xticklabels =names(ellipses.posterior) , 
                 xlab = c(""),
                 ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                 bty = "L",
                 las = 1,
                 main = "Community | Group",ylims = c(0,1.22),ct=c("median")
                # clr = my_clrs
)
# Add red x's for the ML estimated SEA-c
#points(1:ncol(SEA.B), group.ML[3,], col="red", pch = "x", lwd = 2)
dev.off()


SEA.B.db <- as.data.frame(SEA.B)
boxplot(SEA.B.db)
t.test(SEA.B.db$V5, SEA.B.db$V6)
t.test(SEA.B.db$V7, SEA.B.db$V8)
t.test(SEA.B.db$V9, SEA.B.db$V10)
t.test(SEA.B.db$V11, SEA.B.db$V12)

#########################################
# Comparing the posterior distributions
########################################
#In order to test whether one species/comunity ellipse is smaller or larger than another, we can simply calculate the probability that its posterior distribution is smaller (or larger). This is achieved by comparing each pair of posterior draws for both groups, and determining which is smaller in magnitude. We then find the proportion of draws that are smaller, and this is a direct proxy for the probability that one group’s posterior distribution (of ellipse size in this case) is smaller than the other."
#At exactly 50% the two distributions would have identical medians. 

Eknas_brygga.comp <- sum( SEA.B[,6] < SEA.B[,5] ) / nrow(SEA.B)
print(Eknas_brygga.comp)

Vretenvägen.comp <- sum( SEA.B[,8] < SEA.B[,7] ) / nrow(SEA.B)
print(Vretenvägen.comp)

Skalsmara.comp <- sum( SEA.B[,10] < SEA.B[,9] ) / nrow(SEA.B)
print(Skalsmara.comp)

Tranarovagen.comp <- sum( SEA.B[,11] < SEA.B[,12] ) / nrow(SEA.B)
print(Tranarovagen.comp)

probability_BMsmaller <- c(Eknas_brygga.comp,Vretenvägen.comp,Skalsmara.comp,Tranarovagen.comp)
result <- as.data.frame(probability_BMsmaller)
names(ellipses.posterior)
result$Location <- unique(mydatasb$community)[5:8]; result #At exactly 50% the two distributions would have identical medians. 

#############################################################################
# the overlap in niche size between area
#############################################################################
# Eknas_brygga
overlap.Eknas_brygga <- maxLikOverlap("Eknas_brygga.Mytilopsis", "Eknas_brygga.BM", siber.example, p = 0.95, n =);print(overlap.Eknas_brygga)
prop.of.both.Eknas_brygga <- as.numeric(overlap.Eknas_brygga["overlap"] / (overlap.Eknas_brygga["area.1"] + overlap.Eknas_brygga["area.2"]))
print(prop.of.both.Eknas_brygga)

# Vretenvägen
overlap.Vretenvägen <- maxLikOverlap("Vretenvägen.Mytilopsis", "Vretenvägen.BM", siber.example, p = 0.95, n =);print(overlap.Vretenvägen)
prop.of.both.Vretenvägen <- as.numeric(overlap.Vretenvägen["overlap"] / (overlap.Vretenvägen["area.1"] + overlap.Vretenvägen["area.2"]))
print(prop.of.both.Vretenvägen)

# Skalsmara
overlap.Skalsmara<- maxLikOverlap("Skalsmara.Mytilopsis", "Skalsmara.BM", siber.example, p = 0.95, n =);print(overlap.Skalsmara)
prop.of.both.Skalsmara <- as.numeric(overlap.Skalsmara["overlap"] / (overlap.Skalsmara["area.1"] + overlap.Skalsmara["area.2"]))
print(prop.of.both.Skalsmara)

# Tranarovagen
overlap.Tranarovagen <- maxLikOverlap("Tranarovagen.Mytilopsis", "Tranarovagen.BM", siber.example, p = 0.95, n =);print(overlap.Tranarovagen)
prop.of.both.Tranarovagen<- as.numeric(overlap.Tranarovagen["overlap"] / (overlap.Tranarovagen["area.1"] + overlap.Tranarovagen["area.2"]))
print(prop.of.both.Tranarovagen)

Overlap_prop <-  c(prop.of.both.Eknas_brygga,prop.of.both.Vretenvägen,prop.of.both.Skalsmara,prop.of.both.Tranarovagen)
Overlap_prop <- as.data.frame(Overlap_prop)
result$Overlap_prop <- Overlap_prop$Overlap_prop
result <- result[, c(2, 1,  3)];result
write.csv(as.data.frame(result), "result.csv")

# plot  overlap in niche size between area
jpeg("Biplot_by Area.jpeg",width = 250, height = 200, units = "mm", res = 600)
ggplot(mydata %>% dplyr::filter(LOCATION %in% c(unique(mydatasb$community)[5:8])), aes(d13C,d15N))+ theme_bw()  + geom_point(aes(color = Species, shape = Species), size=2.2) +theme(legend.position="left")+theme(legend.position="right") +
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
  geom_errorbar(data = sbg%>% dplyr::filter(LOCATION %in% c(unique(mydatasb$community)[5:8])), 
                mapping = aes(x = mC, y = mN,
                              ymin = mN - 1.96*sdN, 
                              ymax = mN + 1.96*sdN), 
                width = 0) +
  geom_errorbarh(data = sbg%>% dplyr::filter(LOCATION %in% c(unique(mydatasb$community)[5:8])), 
                 mapping = aes(x = mC, y = mN,
                               xmin = mC - 1.96*sdC,
                               xmax = mC + 1.96*sdC),
                 height = 0) + 
  geom_point(data = sbg%>% dplyr::filter(LOCATION %in% c(unique(mydatasb$community)[5:8])), aes(x = mC,   y = mN,
                             fill = Species,shape = Species, color = Species,size=3),
             alpha = 0.9, show.legend = FALSE) 
dev.off()



##################################
# nicheROVER method
#################################
library(nicheROVER)

# niche overlap plots for 95% niche region sizes
listoverlap <-  list()
for (e  in unique(mydata$LOCATION)[5:8]) {
mydataoverl <- mydata  %>% dplyr::filter(LOCATION == e ) 
nsamples <- 1000
data.par <- tapply(1:nrow(mydataoverl), mydataoverl$Species,
                   function(ii) niw.post(nsamples = nsamples, X = mydataoverl[ii,5:6]))
over.stat <- overlap(data.par, nreps = nsamples, nprob = 1000, alpha = c(.95))
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
