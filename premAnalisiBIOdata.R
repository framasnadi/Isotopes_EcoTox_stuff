####################################################'
## Herring bio and bulk data plotting and Prelim Analyses
####################################################
library(readxl)
library(ggplot2)
library(dplyr)
library(visreg)
library(openxlsx)
library(corrplot)
library(tidyr)
library(gridExtra)
library(ggridges)
library(ggrepel)
library(plotrix)
library(psych)
library("PerformanceAnalytics")
library(SIBER)
setwd("C:/Users/frma6502/Desktop/SU_Postdoc/Herring Formas/Analisi")
pd <- position_dodge(0.5)

# create common DB
##################
### AREA = LAND + ANG
################
# create Ang DB
bio_ang <- read_excel("RAW.bio_bulk.xlsx", 
                           sheet = "Biologdata ang");str(bio_ang)
bulk_ang <- read_excel("RAW.bio_bulk.xlsx", 
                      sheet = "bulk_ang");str(bulk_ang)
db.ang <- left_join(bio_ang, bulk_ang)
db.ang <- db.ang %>% dplyr::select(ID, Year, Area, Season, TL,wg, Age, Sex, d13C, d15N, "N%", "C%")

# create Land DB
bio_land <- read_excel("RAW.bio_bulk.xlsx", 
                      sheet = "biodata land");str(bio_land)
bio_land$TL <- as.numeric(bio_land$TL)
bio_land$Year <- as.numeric(bio_land$Year)
bulk_land <- read_excel("RAW.bio_bulk.xlsx", 
                       sheet = "bulk_land");str(bulk_land)
db.land <- left_join(bio_land, bulk_land)
db.land <- db.land %>% dplyr::select(ID, Year, Area,Season,  TL,wg, Age, Sex, d13C, d15N, "N%", "C%")

# combine dbs
db.biobulk <- rbind(db.ang, db.land) 
summary(db.biobulk)
# save db 
#write.xlsx(db.biobulk, "db.biobulkl.xlsx")



##################################################################
# from here if db already available
##################################################################
db.biobulk <- read_excel("db.biobulk.xlsx")
db.biobulk <- db.biobulk %>% dplyr::filter(d15N > 0)

# sample size db by Year
smplsz <- db.biobulk %>% group_by(Area, Year, Season) %>%  dplyr::count(Year)
ggplot(db.biobulk , aes(x=as.numeric(Year), fill=Area, color=Area)) +
  geom_histogram(position="dodge", alpha=0.7,binwidth = 1) +theme(legend.position="bottom")+facet_grid(~Season)+ ggtitle(paste("Smpl.size"))

# PLOT age-leght
jpeg(paste("LAAtot_byArea_biodb.jpeg"),width = 300, height = 200, units = "mm", res = 600)
p1 <- ggplot(db.biobulk,aes(x = as.factor(Age), y = TL, fill = as.factor(Age))) + geom_boxplot()+theme(legend.position="")+facet_grid(~Area*Season)
p2 <- ggplot(db.biobulk , aes(x=TL, fill=as.factor(Age), color=as.factor(Age))) +
  geom_histogram(position="stack", alpha=0.7,binwidth = 1) +theme(legend.position="")+facet_grid(~Area*Season)+ ggtitle(paste("L-A by Area Bio db"))
grid.arrange( p2, p1,nrow = 2)
dev.off()


# PLOT geom_density_ridges
jpeg(paste("LFD_years_byArea_biodb.jpeg"),width = 300, height = 200, units = "mm", res = 600)
totavg <- db.biobulk %>% group_by(Area, Season) %>% dplyr::summarize(  avg.TL = geometric.mean(TL,na.rm = T)) ;totavg
ggplot(db.biobulk) +
  geom_density_ridges(aes(x =TL , y = as.factor(Year),
                          group = ( as.factor(Year)),
                          fill = as.factor(Year),height = stat(count)), stat="binline",binwidth = 0.5,alpha =  0.4,rel_min_height = 0.00001,scale=3)+theme(legend.position="")+ geom_vline(data  = totavg, aes(xintercept = avg.TL ),linetype = "dashed") +facet_grid(~Area*Season)+ ggtitle(paste("LFD by Area Bio db"))
dev.off()

# PLOT mean age-leght by Year
dbbulk.mean <- db.biobulk %>% group_by(Area, Year,Season) %>% dplyr::summarize(SE.TL = std.error(TL,na.rm = T),  avg.TL = geometric.mean(TL,na.rm = T),  SE.Age = std.error(Age,na.rm = T) ,avg.Age = geometric.mean(Age,na.rm = T),  SE.N = std.error(d15N,na.rm = T),  avg.d15N = geometric.mean(d15N,na.rm = T), SE.C = -std.error(-d13C,na.rm = T),  avg.d13C = -geometric.mean(-d13C,na.rm = T)) 

jpeg(paste("meanLenght_Age_byArea_biodb2.jpeg"),width = 300, height = 200, units = "mm", res = 600)
p1 <- ggplot(dbbulk.mean,aes(x = as.factor(Year), y = avg.TL, group = Area, color=Area )) +  geom_errorbar(aes(ymin=avg.TL-SE.TL, ymax=avg.TL+SE.TL), width=.1, position=pd)+ geom_point(position=pd)+theme(legend.position="")+theme(legend.position="")+ geom_smooth(method = "loess", se = F, linetype="solid", size= 0.7) +facet_wrap(~Season) + theme(axis.text.x = element_text(angle=90, hjust=1))+ geom_hline(yintercept = 18, linetype = "dashed")+ ggtitle(paste("mean L by Area Bio db"))
p2 <- ggplot(dbbulk.mean,aes(x = as.factor(Year), y = avg.Age, group = Area, color=Area )) +  geom_errorbar(aes(ymin=avg.Age-SE.Age, ymax=avg.Age+SE.Age), width=.1, position=pd)+ geom_point(position=pd)+theme(legend.position="")+theme(legend.position="bottom")+ geom_smooth(method = "loess", se = F, linetype="solid", size= 0.7) +facet_wrap(~Season) + theme(axis.text.x = element_text(angle=90, hjust=1))+ ggtitle(paste("mean Age by Area Bio db"))
grid.arrange( p1, p2,nrow = 2)
dev.off()

# d15N over time
jpeg(paste("bulks_byArea_biodb.jpeg"),width = 300, height = 200, units = "mm", res = 600)
p1 <- ggplot(dbbulk.mean,aes(x = as.factor(Year), y = avg.d15N, group = Area, color=Area )) +  geom_errorbar(aes(ymin=avg.d15N-SE.N, ymax=avg.d15N+SE.N), width=.1, position=pd)+ geom_point(position=pd)+theme(legend.position="")+theme(legend.position="")+ geom_smooth(method = "loess", se = F, linetype="solid", size= 0.7) +facet_wrap(~Season) + theme(axis.text.x = element_text(angle=90, hjust=1))+ ggtitle(paste("mean d15N by Area"))
p2 <- ggplot(dbbulk.mean,aes(x = as.factor(Year), y = avg.d13C, group = Area, color=Area )) +  geom_errorbar(aes(ymin=avg.d13C-SE.C, ymax=avg.d13C+SE.C), width=.1, position=pd)+ geom_point(position=pd)+theme(legend.position="")+theme(legend.position="bottom")+ geom_smooth(method = "loess", se = F, linetype="solid", size= 0.7) +facet_wrap(~Season) + theme(axis.text.x = element_text(angle=90, hjust=1))+ ggtitle(paste("mean d13C by Area"))
grid.arrange( p1, p2,nrow = 2)
dev.off()

# Adjust d15N for TL follwing Burgess et al 2013 
db.CONT.avgTEMP <- dbbulk.mean 
db.CONT.avgTEMP$Area.Season <- paste(db.CONT.avgTEMP$Area, db.CONT.avgTEMP$Season)
plotAdjTLKENDAL <- paste("plot_BIOBulk/AdjTLcomparison/", sep="")
dir.create(plotAdjTLKENDAL)
for  (j in unique(db.CONT.avgTEMP$Area.Season)   )       {
  CONT1 <- db.CONT.avgTEMP %>% dplyr::filter(Area.Season == j )
  mod1 <- lm(avg.d15N ~   avg.TL , data= CONT1) ; summary(mod1)
  A <- mod1[["coefficients"]][2];print(A)
  pvalue <- summary(mod1)$coefficients[2,4]  ;print(pvalue)
  if (pvalue < 0.051) {   # plot only if the lm is significant!
    p1 <- ggplot(CONT1,aes(x=avg.d15N,y=avg.TL)) + 
      geom_point() + geom_smooth(method="lm",col="blue")+ggtitle(paste("Value vs Lenght" , j)); print(p1)
    CONT1$d15N_adj <- CONT1$avg.d15N + A*(mean(CONT1$avg.TL, na.rm = T) - CONT1$avg.TL)
    CONT1$TL_Adjustment.d15N <- "Yes"
    #   p2 <-ggplot(CONT1) + geom_point(aes(Year, avg.Value)) + geom_line(aes(Year, avg.Value))+ geom_point(aes(Year, Value_adj), color="red") + geom_line(aes(Year, Value_adj), color="red")+ylab("value")+ggtitle(paste("Original vs Adj_value ", i , j)); print(p2)
  } else {CONT1$d15N_adj <- CONT1$avg.d15N 
  CONT1$TL_Adjustment.d15N <- "No"}
  write.csv(CONT1,paste(plotAdjTLKENDAL,"d15N",j ,".csv", sep=""),row.names=FALSE)}

my_list <- list.files(plotAdjTLKENDAL, full.names=T) # delete the temporal db in the folder for space reason
df <- readr::read_csv(my_list, id = "file_name");df <- df %>% dplyr::select(-file_name)
table(df$TL_Adjustment.d15N,df$Area.Season)

# WG by AREA.Season (comaprison Adj vs original)
for  (j in unique(df$Area.Season)   )       {
  jpeg(paste( plotAdjTLKENDAL , "d15N_Adj_biodata_" ,j,  ".jpeg"),width = 400, height = 200, units = "mm", res = 600)
  AA <- ggplot(df  %>% dplyr::filter(Area.Season ==j), aes(x = as.factor(Year), y = d15N_adj ))  + geom_point(color="black") +geom_line(aes(group=TL_Adjustment.d15N),color="black") +geom_line(aes(x = as.factor(Year), y = avg.d15N , group=TL_Adjustment.d15N),color="black", linetype = "dashed")   + ggtitle(paste(j))+ theme(axis.text.x = element_text(angle=90, hjust=1))+ ylab("Value") + xlab("Year"); print(AA)
  dev.off()
}
dff <- df

# Adjust d13C for TL follwing Burgess et al 2013 
for  (j in unique(db.CONT.avgTEMP$Area.Season)   )       {
  CONT1 <- db.CONT.avgTEMP %>% dplyr::filter(Area.Season == j )
  mod1 <- lm(avg.d13C ~   avg.TL , data= CONT1) ; summary(mod1)
  A <- mod1[["coefficients"]][2];print(A)
  pvalue <- summary(mod1)$coefficients[2,4]  ;print(pvalue)
  if (pvalue < 0.051) {   # plot only if the lm is significant!
    p1 <- ggplot(CONT1,aes(x=avg.d13C,y=avg.TL)) + 
      geom_point() + geom_smooth(method="lm",col="blue")+ggtitle(paste("Value vs Lenght" , j)); print(p1)
    CONT1$d13C_adj <- CONT1$avg.d13C + A*(mean(CONT1$avg.TL, na.rm = T) - CONT1$avg.TL)
    CONT1$TL_Adjustment.d13C <- "Yes"
    #   p2 <-ggplot(CONT1) + geom_point(aes(Year, avg.Value)) + geom_line(aes(Year, avg.Value))+ geom_point(aes(Year, Value_adj), color="red") + geom_line(aes(Year, Value_adj), color="red")+ylab("value")+ggtitle(paste("Original vs Adj_value ", i , j)); print(p2)
  } else {CONT1$d13C_adj <- CONT1$avg.d13C 
  CONT1$TL_Adjustment.d13C <- "No"}
  write.csv(CONT1,paste(plotAdjTLKENDAL,"d13C",j ,".csv", sep=""),row.names=FALSE)}

my_list <- list.files(plotAdjTLKENDAL, full.names=T) # delete the temporal db in the folder for space reason
my_list <- my_list[4:6]
df <- readr::read_csv(my_list, id = "file_name");df <- df %>% dplyr::select(-file_name)
table(df$TL_Adjustment.d13C,df$Area.Season)

# WG by AREA.Season (comaprison Adj vs original)
for  (j in unique(df$Area.Season)   )       {
  jpeg(paste( plotAdjTLKENDAL , "d13C_Adj_biodata_" ,j,  ".jpeg"),width = 400, height = 200, units = "mm", res = 600)
  AA <- ggplot(df  %>% dplyr::filter(Area.Season ==j), aes(x = as.factor(Year), y = d13C_adj ))  + geom_point(color="black") +geom_line(aes(group=TL_Adjustment.d13C),color="black") +geom_line(aes(x = as.factor(Year), y = avg.d13C , group=TL_Adjustment.d13C),color="black", linetype = "dashed")   + ggtitle(paste(j))+ theme(axis.text.x = element_text(angle=90, hjust=1))+ ylab("Value") + xlab("Year"); print(AA)
  dev.off()
}
db.d15N.d13C_Adj <- left_join(dff, df)

# Z-score normalization
Zscoredb <- db.d15N.d13C_Adj %>% group_by(Area.Season) %>% dplyr::summarize(d15N.meanZ.val = mean(d15N_adj,na.rm = T),  d15N.SDZ.val = sd(d15N_adj,na.rm = T),   d13C.meanZ.val = mean(d13C_adj,na.rm = T),  d13C.SDZ.val = sd(d13C_adj,na.rm = T)) 
db.d15N.d13C_Adj <- left_join(db.d15N.d13C_Adj, Zscoredb)
db.d15N.d13C_Adj$Z.value.d15N_adj <- (db.d15N.d13C_Adj$d15N_adj - db.d15N.d13C_Adj$d15N.meanZ.val) / db.d15N.d13C_Adj$d15N.SDZ.val
db.d15N.d13C_Adj$Z.value.d13C_adj <- (db.d15N.d13C_Adj$d13C_adj - db.d15N.d13C_Adj$d13C.meanZ.val) / db.d15N.d13C_Adj$d13C.SDZ.val
#write.xlsx(db.d15N.d13C_Adj, "db.d15N.d13C_Adj.xlsx")


##############
# SIBER pckg
#############
db.biobulk <- read_excel("db.biobulk.xlsx")
db.biobulk <- db.biobulk %>% dplyr::filter(d15N > 0)
# Summarise By Group (sbg)
sbg <- db.biobulk %>% 
  group_by(Year, Area, Season) %>% na.omit() %>% 
  summarise(count = n(),
            mC = mean(d13C), 
            sdC = sd(d13C), 
            mN = mean(d15N), 
            sdN = sd(d15N))

# biplot
jpeg("Biplot_bulk.jpeg",width = 450, height = 230, units = "mm", res = 600)
ggplot(sbg, aes(mC,mN, group=Year))+ theme_bw()  + 
 # geom_point(aes(color = Year, shape = Area), size=2.2)+
  theme(legend.position="left")+theme(legend.position="right")  + 
 # stat_ellipse(aes(group = interaction(Year, Area), 
#                   fill = Year, 
#                   color = Year), 
#               alpha = 0.25, 
#               level = 0.95,
#               type = "norm",
 #              geom = "polygon") +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  geom_errorbar(data = sbg, 
                mapping = aes(x = mC, y = mN,
                              ymin = mN - 1.96*sdN, 
                              ymax = mN + 1.96*sdN,color = Year), 
                width = 0) +
  geom_errorbarh(data = sbg, 
                 mapping = aes(x = mC, y = mN,
                               xmin = mC - 1.96*sdC,
                               xmax = mC + 1.96*sdC,color = Year),
                 height = 0) + 
  geom_point(data = sbg, aes(x = mC, 
                             y = mN,
                             fill = Year,shape = Area, color = Year),size=3,
             alpha = 0.9, show.legend = T)  + 
  geom_text_repel(aes(mC,mN, color=Year,label = Year),                                                                              box.padding   = 0.55,                                                                               point.padding = 0.5,                                                                             segment.color = 'grey50'  )+
  facet_grid(~Area*Season)
dev.off()

################################################################################
#       Layman metrics (loop by Year)
################################################################################
Leymar.LIST <- list()
sito <- "Ängskärsklubb" # "Ängskärsklubb" "Landsort"
stg <- "Spring"  # "Fall" "Spring"
for (i in unique(db.biobulk$Year)   )   { 
  db.temp <- as.data.frame( as.data.frame(db.biobulk) %>% dplyr::filter(Area == sito)  %>% dplyr::filter(Season == stg) %>% dplyr::filter(Year == i)) 
  Leymar.LIST[i] <- list(laymanMetrics(db.temp$d13C, db.temp$d15N)$metrics)
  layman.db <- as.data.frame(do.call(cbind, Leymar.LIST)) ;layman.db$metrics <- c("d15N_range","d13C_range","TA","CD","NND","SDNND")
}  
layman.db.ANG.spring  <- layman.db  %>%    pivot_longer(
  cols = c(1:35) ,  # pivot all columns except case_id 
  names_to = "Year",    # assign name for new column that holds the symptoms
  values_to = "Value"); layman.db.ANG.spring$Area <- sito; layman.db.ANG.spring$Season <- stg

# combine differnt db
layman.db.fin <- rbind(layman.db.LAND.fall,layman.db.ANG.fall,layman.db.ANG.spring)%>% 
  mutate(Value = replace(Value, Value %in%c("-Inf","NaN"), NA))
# save db 
#write.xlsx(layman.db.fin, "layman.db.fin.xlsx")

# 
layman.db.fin$Area.Season <-  paste(layman.db.fin$Area, layman.db.fin$Season)
jpeg("Layman_Timeseries_ggplot.jpeg",width = 450, height = 200, units = "mm", res = 600)
ggplot( layman.db.fin  , aes(Year,Value, group=Area.Season ,color=Area.Season))  + geom_point(position=pd)+ geom_line(linetype = "dashed", position=pd) +theme(legend.position="bottom")+ geom_smooth(method = "loess", se = F, linetype="solid", size= 0.7)+ facet_wrap(~metrics, scales = "free")+ theme(axis.text.x = element_text(angle=90, hjust=1))+ ggtitle(paste("Layman metrics")) + xlab("Year")
dev.off()

################################################################################
#       Stable Isotope Bayesian Ellipses (Year as group)
################################################################################
# create the siber object for LAND and ANG separately 

# Landsort
dbbulk.siber.LAND  <- as.data.frame( db.biobulk %>% dplyr::filter(Area == "Landsort") %>%  dplyr::rename(iso2 = d15N  , iso1 = d13C, group = Year ,community= Season) %>% dplyr::select(iso1,iso2, group, community))
siber.LAND <- createSiberObject(dbbulk.siber.LAND)
siber.LAND$sample.sizes

# ellipses and group.hulls are set to TRUE or T for short to force their plotting. 
plotSiberObject(siber.LAND,
                ax.pad = 0.15, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = F, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030')
)

# Calculate sumamry statistics for each group: TA, SEA and SEAc
group.ML.LAND <- groupMetricsML(siber.LAND)
legend("topright", colnames(group.ML.LAND),
       pch = c(1), col = c(1:32), lty = 0.2)

group.ML.LAND <- as.data.frame(group.ML.LAND)
group.ML.LAND$metrics <- c("TA","SEA","SEAc")

group.ML.LAND <- group.ML.LAND %>%  pivot_longer(
  cols = c(1:32) ,  # pivot all columns except case_id 
  names_to = "group",    # assign name for new column that holds the symptoms
  values_to = "Value") 
group.ML.LAND$Area <- c("Landsort")
group.ML.LAND$Year <- stringr::str_remove(group.ML.LAND$group, pattern = "Fall.")
group.ML.LAND$Season <- "Fall"
print(group.ML.LAND)
# add a legend

######################################################################
## Using Bayesian Inference to calculate uncertainty around ellipses
######################################################################
"So far these still just point-metrics that describe the width of the isotopic niche. That is, they are single numbers for each group, which means that we can’t compare one group to another in a statistical sense as we lack a measure of the uncertainty around each estimate. This is where we can use Bayesian Inference to quantify the error associated with fitting these ellipses to each group, that arises from both the number of samples we have, and also their distribution"

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
ellipses.posterior.LAND <- siberMVN(siber.LAND, parms, priors)

# The posterior estimates of the ellipses for each group can be used to
# calculate the SEA.B for each group.
SEA.B.LAND <- siberEllipses(ellipses.posterior.LAND)

jpeg("Niche_Timeseries_LAND.jpeg",width = 300, height = 150, units = "mm", res = 600)
siberDensityPlot(SEA.B.LAND, xticklabels = colnames(groupMetricsML(siber.LAND)), 
                 xlab = c("Year"),ylim = c(0,15),
                 ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                 bty = "L",
                 las = 1,
                 main = "SIBER ellipses by Year - Landsort"
)
# Add red x's for the ML estimated SEA-c
dev.off()


###########################################################################
# Ängskärsklubb
dbbulk.siber.ANG <- as.data.frame( db.biobulk %>% dplyr::filter(Area == "Ängskärsklubb") %>% dplyr::filter(Season == "Fall") %>%  dplyr::rename(iso2 = d15N  , iso1 = d13C, group = Year ,community= Season) %>% dplyr::select(iso1,iso2, group, community))
siber.ANG <- createSiberObject(dbbulk.siber.ANG)

# ellipses and group.hulls are set to TRUE or T for short to force their plotting. 
plotSiberObject(siber.ANG,
                ax.pad = 1, 
                hulls = F, community.hulls.args, 
                ellipses = F, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030')
)

# Calculate sumamry statistics for each group: TA, SEA and SEAc
group.ML.ANG <- groupMetricsML(siber.ANG)
group.ML.ANG <- as.data.frame(group.ML.ANG)
group.ML.ANG$metrics <- c("TA","SEA","SEAc")

group.ML.ANG.spring <- group.ML.ANG %>%  pivot_longer(
  cols = c(1:18) ,  # pivot all columns except case_id 
  names_to = "group",    # assign name for new column that holds the symptoms
  values_to = "Value") 
group.ML.ANG.spring$Area <- c("Ängskärsklubb")
group.ML.ANG.spring$Year <- stringr::str_remove(group.ML.ANG.spring$group, pattern = "Spring.")
group.ML.ANG.spring$Season <- "Spring"
print(group.ML.ANG.spring)

group.ML.ANG.fall <- group.ML.ANG %>%  pivot_longer(
  cols = c(1:20) ,  # pivot all columns except case_id 
  names_to = "group",    # assign name for new column that holds the symptoms
  values_to = "Value") 
group.ML.ANG.fall$Area <- c("Ängskärsklubb")
group.ML.ANG.fall$Year <- stringr::str_remove(group.ML.ANG.fall$group, pattern = "Fall.")
group.ML.ANG.fall$Season <- "Fall"
print(group.ML.ANG.fall)

# add a legend
legend("topright", colnames(group.ML.ANG),
       pch = c(2,1,2), col = c(1:2, 1:2), lty = 0.2)

ellipses.posterior.ANG <- siberMVN(siber.ANG, parms, priors)

# The posterior estimates of the ellipses for each group can be used to
# calculate the SEA.B for each group.
SEA.B.ANG <- siberEllipses(ellipses.posterior.ANG)

jpeg("Niche_Timeseries_ANG.spring.jpeg",width = 300, height = 150, units = "mm", res = 600)
siberDensityPlot(SEA.B.ANG, xticklabels = colnames(group.ML.ANG), 
                 xlab = c("Year"),
                 ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                 bty = "L",
                 las = 1,
                 main = "SIBER ellipses by Year - Ängskärsklubb SPRING"
)
# Add red x's for the ML estimated SEA-c
dev.off()

#############################
# ggplot per comparison finale
#############################
db.niche.fin <- rbind(group.ML.ANG.fall, group.ML.ANG.spring, group.ML.LAND)
# save db 
#write.xlsx(db.niche.fin, "db.niche.fin.xlsx")

#
db.niche.fin <- read_excel("db.niche.fin.xlsx")
db.niche.fin$Area.Season <-  paste(db.niche.fin$Area, db.niche.fin$Season)

jpeg("Niche_Timeseries_ggplot.jpeg",width = 300, height = 150, units = "mm", res = 600)
ggplot(db.niche.fin %>% dplyr::filter(metrics == "SEAc") ,aes(x = as.factor(Year), y = Value, group = Area.Season, color=Area.Season )) + geom_point(position=pd)+ geom_line(linetype = "dashed", position=pd)+theme(legend.position="")+theme(legend.position="bottom")+ geom_smooth(method = "loess", se = F, linetype="solid", size= 0.7)  + theme(axis.text.x = element_text(angle=90, hjust=1))+ ggtitle(paste("Isotopic niches by Area")) + xlab("Year")+ ylab(expression("Standard Ellipse Area " ('\u2030' ^2) ))
dev.off()



# TL adjustment on LAYMAN
layman_db_fin <- read_excel("layman.db.fin.xlsx")
layman_db_fin$Area.Season <- paste(layman_db_fin$Area,layman_db_fin$Season)
layman_db_fin$Year <- as.numeric(layman_db_fin$Year )
tl <- dddd %>% select(Area.Season,Year,avg.TL)
db_trophic <- left_join(layman_db_fin , tl)


plotAdjTLKENDAL <- paste("plot_BIOBulk/AdjTLcomparison_metrics/", sep="")
dir.create(plotAdjTLKENDAL)
for (i in unique(db_trophic$metrics)  )   {
  for  (j in unique(db_trophic$Area.Season)   )       {
    CONT1 <- db_trophic %>% dplyr::filter(metrics == i ) %>% dplyr::filter(Area.Season == j )
    mod1 <- lm(Value ~   avg.TL , data= CONT1) ; summary(mod1)
    A <- mod1[["coefficients"]][2];print(A)
    pvalue <- summary(mod1)$coefficients[2,4]  ;print(pvalue)
    if (pvalue < 0.051) {   # plot only if the lm is significant!
      # p1 <- ggplot(CONT1,aes(x=Value,y=avg.TL)) + 
      #   geom_point() + geom_smooth(method="lm",col="blue")+ggtitle(paste("Value vs Lenght", i , j)); print(p1)
      CONT1$Value_adj <- CONT1$Value + A*(mean(CONT1$avg.TL, na.rm = T) - CONT1$avg.TL)
      CONT1$TL_Adjustment.layman <- "Yes"
    } else {CONT1$Value_adj <- CONT1$Value 
    CONT1$TL_Adjustment.layman <- "No"}
    write.csv(CONT1,paste(plotAdjTLKENDAL,"db",i,j ,".csv", sep=""),row.names=FALSE)}
}

my_list <- list.files(plotAdjTLKENDAL, full.names=T) # delete the temporal db in the folder for space reason
df <- readr::read_csv(my_list, id = "file_name");df <- df %>% dplyr::select(-file_name)
table(df$TL_Adjustment.layman,df$Area.Season)
dfYES <- df %>% filter(TL_Adjustment.layman == "Yes"); table(dfYES$metrics,dfYES$Area.Season)

# by Subgroup and by AREA.Season (comaprison Adj vs original)
for (i in unique(df$metrics)   )   {
  for  (j in unique(df$Area.Season)   )       {
    jpeg(paste( plotAdjTLKENDAL , "Adj_layman.data_" ,i,j,  ".jpeg"),width = 400, height = 200, units = "mm", res = 600)
    AA <- ggplot(df %>% dplyr::filter(metrics ==i) %>% dplyr::filter(Area.Season ==j), aes(x = as.factor(Year), y = Value_adj ))  + geom_point(color="red") +geom_line(aes(group=TL_Adjustment.layman),color="red") +geom_line(aes(x = as.factor(Year), y = Value , group=TL_Adjustment.layman),color="black") +geom_point(aes(x = as.factor(Year), y = Value , group=TL_Adjustment.layman),color="black") +geom_hline(yintercept = 0, linetype = "dotted") +facet_wrap(~metrics, scales = "free") + ggtitle(paste(i,j))+ theme(axis.text.x = element_text(angle=90, hjust=1))+ ylab("Value") + xlab("Year"); print(AA)
    dev.off()
  }
}

# Z-score normalization
Zscoredb <- df %>% group_by(Area.Season,metrics) %>% dplyr::summarize(meanZ.val = mean(Value_adj,na.rm = T),  SDZ.val = sd(Value_adj,na.rm = T)) 
df <- left_join(df, Zscoredb)
df$layman.Z.Value <- (df$Value_adj - df$meanZ.val) / df$SDZ.val
# save db
#write.xlsx(df, "layman.db.fin_Zvalue.xlsx")



# TL adjustment on SEAc
db.niche.fin <- read_excel("db.niche.fin.xlsx")
db.niche.fin$Area.Season <- paste(db.niche.fin$Area,db.niche.fin$Season)
db.niche.fin$Year <- as.numeric(db.niche.fin$Year )
db.niche.fin <-db.niche.fin %>% filter(metrics == "SEAc")
db.niche.fin$avg.TL <- tl$avg.TL

plotAdjTLKENDAL <- paste("plot_BIOBulk/AdjTLcomparison_metrics/", sep="")
dir.create(plotAdjTLKENDAL)
for (i in unique(db.niche.fin$metrics)  )   {
  for  (j in unique(db.niche.fin$Area.Season)   )       {
    CONT1 <- db.niche.fin %>% dplyr::filter(metrics == i ) %>% dplyr::filter(Area.Season == j )
    mod1 <- lm(Value ~   avg.TL , data= CONT1) ; summary(mod1)
    A <- mod1[["coefficients"]][2];print(A)
    pvalue <- summary(mod1)$coefficients[2,4]  ;print(pvalue)
    if (pvalue < 0.051) {   # plot only if the lm is significant!
      # p1 <- ggplot(CONT1,aes(x=Value,y=avg.TL)) + 
      #   geom_point() + geom_smooth(method="lm",col="blue")+ggtitle(paste("Value vs Lenght", i , j)); print(p1)
      CONT1$Value_adj <- CONT1$Value + A*(mean(CONT1$avg.TL, na.rm = T) - CONT1$avg.TL)
      CONT1$TL_Adjustment.layman <- "Yes"
    } else {CONT1$Value_adj <- CONT1$Value 
    CONT1$TL_Adjustment.layman <- "No"}
    write.csv(CONT1,paste(plotAdjTLKENDAL,"db",i,j ,".csv", sep=""),row.names=FALSE)}
}

my_list <- list.files(plotAdjTLKENDAL, full.names=T) # delete the temporal db in the folder for space reason
df <- readr::read_csv(my_list, id = "file_name");df <- df %>% dplyr::select(-file_name)
table(df$TL_Adjustment.layman,df$Area.Season)
dfYES <- df %>% filter(TL_Adjustment.layman == "Yes"); table(dfYES$metrics,dfYES$Area.Season)

# by Subgroup and by AREA.Season (comaprison Adj vs original)
for (i in unique(df$metrics)   )   {
  for  (j in unique(df$Area.Season)   )       {
    jpeg(paste( plotAdjTLKENDAL , "Adj_SEAc.data_" ,i,j,  ".jpeg"),width = 400, height = 200, units = "mm", res = 600)
    AA <- ggplot(df %>% dplyr::filter(metrics ==i) %>% dplyr::filter(Area.Season ==j), aes(x = as.factor(Year), y = Value_adj ))  + geom_point(color="red") +geom_line(aes(group=TL_Adjustment.layman),color="red") +geom_line(aes(x = as.factor(Year), y = Value , group=TL_Adjustment.layman),color="black") +geom_point(aes(x = as.factor(Year), y = Value , group=TL_Adjustment.layman),color="black") +geom_hline(yintercept = 0, linetype = "dotted") +facet_wrap(~metrics, scales = "free") + ggtitle(paste(i,j))+ theme(axis.text.x = element_text(angle=90, hjust=1))+ ylab("Value") + xlab("Year"); print(AA)
    dev.off()
  }
}

# Z-score normalization
Zscoredb <- df %>% group_by(Area.Season,metrics) %>% dplyr::summarize(meanZ.val = mean(Value_adj,na.rm = T),  SDZ.val = sd(Value_adj,na.rm = T)) 
df <- left_join(df, Zscoredb)
df$SEAc.Z.Value <- (df$Value_adj - df$meanZ.val) / df$SDZ.val
# save db
#write.xlsx(df, "SEAc.db.fin_Zvalue.xlsx")



