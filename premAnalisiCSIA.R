####################################################'
## Herring CSIAA plotting and Prelim Analyses
####################################################
library(readxl)
library(ggplot2)
library(dplyr)
library(visreg)
library(openxlsx)
library(corrplot)
library(tidyr)
library("PerformanceAnalytics")

setwd("D:/SU_Postdoc/Herring Formas/Analisi")
# create herring CSIAA DB
Ang_CSIA_her <- read_excel("Ang_CSIA_her.xlsx") %>% filter (Species == "HER") %>% mutate(Component =  replace(Component, Component == "Glx", "Glu")) %>% mutate(Component =  replace(Component, Component == "Asx", "Asp"))
ref <- read_excel("Ang_CSIA_her.xlsx",  sheet = "ref")
db <- left_join(Ang_CSIA_her, ref)
str(db)
summary(db)

# plotting 500/300  1.666667*200
jpeg(paste("raw15N_by_AA.jpeg"),width = 333, height = 200, units = "mm", res = 600)
ggplot(db) + geom_boxplot(aes(x = as.factor(Year), y = `δ15NAir (‰)`, fill = Season))+theme(legend.position="bottom") + facet_wrap(~Component, scales = "free")+ theme(axis.text.x = element_text(angle=90, hjust=1))
dev.off()


# calculate avevage of the two Analysis
db.fin <-db %>% dplyr::select(Year, Season , Component, `δ15NAir (‰)` ) %>% group_by(Year, Season , Component )  %>% summarise( d15N = mean(`δ15NAir (‰)` ))

#plot ALL AA together
jpeg(paste("d15N_by_AA_byS.jpeg"),width = 333, height = 200, units = "mm", res = 600)
ggplot(db.fin, aes(Year, d15N,colour=Season ))+ geom_point()+ facet_wrap(~Component, scales = "free")+ geom_line()
dev.off()

jpeg(paste("d15N_by_AA_byS_GAM.jpeg"),width = 333, height = 200, units = "mm", res = 600)
ggplot(db.fin, aes(Year, d15N,colour=Season ))+ geom_point()+ facet_wrap(~Component, scales = "free")+ geom_smooth(method = "gam", se = F)
dev.off()

jpeg(paste("d15N_by_AA_byS_loess.jpeg"),width = 333, height = 200, units = "mm", res = 600)
ggplot(db.fin, aes(Year, d15N,colour=Season ))+ geom_point()+ facet_wrap(~Component, scales = "free")+ geom_smooth(method = "loess", se = T)
dev.off()

#plot only PheGlu
jpeg(paste("d15N_by_PheGlu_byS.jpeg"),width = 400, height = 150, units = "mm", res = 600)
ggplot(db.fin %>% filter (Component %in% c("Glu", "Phe")) , aes(Year, d15N,colour=Season ))+ geom_point()+ facet_wrap(~Component, scales = "free")+ geom_line()
dev.off()
jpeg(paste("d15N_by_PheGlu_byS_GAM.jpeg"),width = 400, height = 150, units = "mm", res = 600)
ggplot(db.fin %>% filter (Component %in% c("Glu", "Phe")) , aes(Year, d15N,colour=Season ))+ geom_point()+ facet_wrap(~Component, scales = "free")+ geom_line(linetype="dotted", size= 1)+ geom_smooth(method = "gam", se = T, linetype="solid", size= 0.7)
dev.off()


####################################
# comparison with LAnd CSIA
db.fin
Land.db <- read_excel("Ang_CSIA_her.xlsx",    sheet = "Land")
Land.lon <- Land.db %>%    pivot_longer(
    cols = -c(Year,Season) ,  # pivot all columns except case_id (all the symptoms columns)
    names_to = "Component",    # assign name for new column that holds the symptoms
    values_to = "d15N")
db.fin$Area <- "Ängskärskubb"
Land.lon$Area <- "Landsort" 
comp.db <- rbind(db.fin,Land.lon)

#plots
jpeg(paste("d15N_by_AA_byS_AREA.jpeg"),width = 333, height = 200, units = "mm", res = 600)
ggplot(comp.db %>% filter(Season == "Fall") , aes(Year, d15N,colour=Area ))+ geom_point()+ facet_wrap(~Component, scales = "free")+ geom_line()+  scale_color_manual(values=c("purple","orange"))
dev.off()

jpeg(paste("d15N_by_AA_byS_AREA_GAM.jpeg"),width = 333, height = 200, units = "mm", res = 600)
ggplot(comp.db %>% filter(Season == "Fall") , aes(Year, d15N,colour=Area ))+ geom_point()+ facet_wrap(~Component, scales = "free")+ geom_line(linetype="dotted", size= 1)+ geom_smooth(method = "gam", se = T, linetype="solid", size= 0.7)+  scale_color_manual(values=c("purple","orange"))
dev.off()

jpeg(paste("d15N_by_PheGlu_byS_AREA.jpeg"),width = 400, height = 150, units = "mm", res = 600)
ggplot(comp.db %>% filter(Season == "Fall") %>% filter (Component %in% c("Glu", "Phe")) , aes(Year, d15N,colour=Area ))+  scale_color_manual(values=c("purple","orange"))+facet_wrap(~Component, scales = "free")+ geom_line()+ geom_point()
dev.off()
jpeg(paste("d15N_by_PheGlu_byS_AREA_GAM.jpeg"),width = 400, height = 150, units = "mm", res = 600)
ggplot(comp.db %>% filter(Season == "Fall") %>% filter (Component %in% c("Glu", "Phe")) , aes(Year, d15N,colour=Area ))+ geom_point()+ facet_wrap(~Component, scales = "free")+ geom_line(linetype="dotted", size= 1)+ geom_smooth(method = "gam", se = T, linetype="solid", size= 0.7)+  scale_color_manual(values=c("purple","orange"))
dev.off()


##################################################
# calculate TP based on eq by Chikaraishi et al. 2009 and Nielsen 2015 (piu di una)
db.TP<- comp.db   %>%  pivot_wider(  # pivot all columns except case_id (all the symptoms columns)
  names_from = "Component",    # assign name for new column that holds the symptoms
  values_from = "d15N")
db.TP[4,8]   <- mean(db.TP$His , na.rm = T) # mean value for the His NA 
#  Chikaraishi et al. 2009
db.TP$TP.Chikaraishi <- NA
for (i in 1:length(db.TP$Year)){
  db.TP$TP.Chikaraishi <- ((db.TP$Glu - db.TP$Phe - 3.4)/7.6) + 1
}

#  Nielsen et al. 2015
db.TP$TP.Nielsen <- NA
for (i in 1:length(db.TP$Year)){
  db.TP$TP.Nielsen <- (((db.TP$Glu +db.TP$Ala-0.59+db.TP$Val-3.35+db.TP$Asp-1.78+db.TP$Pro-1.39+db.TP$Ile-2.63+db.TP$Leu-2.93)/7-(db.TP$Phe+db.TP$Lys+0.11+db.TP$Gly+0.27+db.TP$Ser+0.19+db.TP$His+1.23+db.TP$Tyr+1.46+db.TP$Thr+6.72)/7-2.9)/5.9)+1
}

db.TP$TP.GluAla <- NA
for (i in 1:length(db.TP$Year)){
  db.TP$TP.GluAla <- (((db.TP$Glu +db.TP$Ala-0.59)/2-(db.TP$Phe)-2.9)/5.9)+1
}


db.TP$TP.AllTro_Phe <- NA
for (i in 1:length(db.TP$Year)){
  db.TP$TP.AllTro_Phe <- (((db.TP$Glu +db.TP$Ala-0.59+db.TP$Val-3.35+db.TP$Asp-1.78+db.TP$Pro-1.39+db.TP$Ile-2.63+db.TP$Leu-2.93)/7-(db.TP$Phe)-2.9)/5.9)+1
}

db.TP$TP.All_no.norm <- NA
for (i in 1:length(db.TP$Year)){
  db.TP$TP.All_no.norm <- (((db.TP$Glu +db.TP$Ala+db.TP$Val+db.TP$Asp+db.TP$Pro+db.TP$Ile+db.TP$Leu)/7-(db.TP$Phe+db.TP$Lys+db.TP$Gly+db.TP$Ser+db.TP$His+db.TP$Tyr+db.TP$Thr +db.TP$Met)/8-2.9)/5.9)+1
}


#save db
#write.xlsx(db.TP, "db.TP.xlsx")

#plot TP
db.TP.plot<- as.data.frame(db.TP)   %>%  pivot_longer(  # pivot all columns except case_id (all the symptoms columns)
  cols = c(19:23) ,
  names_to = "TP_methods",    # assign name for new column that holds the symptoms
  values_to = "Value")


db.TP.plot$Area.Season <-  paste(db.TP.plot$Area, db.TP.plot$Season)
jpeg(paste("TP_by_methods.jpeg"),width = 300, height = 200, units = "mm", res = 600)
ggplot(db.TP.plot  , aes(Year, Value,colour=Area.Season ))+ geom_point()+ geom_line(linetype="dotted", size= 1)+facet_wrap(~TP_methods, scales = "free")+ geom_smooth(method = "loess", se = F, linetype="solid", size= 0.7)+theme(legend.position="bottom")
dev.off()


jpeg(paste("TP_by_Area.jpeg"),width = 380, height = 180, units = "mm", res = 600)
ggplot(db.TP.plot  , aes(Year, Value,colour=TP_methods ))+ geom_point()+ geom_line(linetype="dotted", size= 1)+facet_wrap(~Area.Season)+ geom_smooth(method = "loess", se = F, linetype="solid", size= 0.7)+theme(legend.position="bottom")+
  theme(legend.text = element_text(size=15))
dev.off()

# Z-score
db.TP.plot.Zscore<- as.data.frame(db.TP)   %>%  pivot_longer(  # pivot all columns except case_id (all the symptoms columns)
  cols = c(4:23) ,
  names_to = "Predictor",    # assign name for new column that holds the symptoms
  values_to = "Value")
db.TP.plot.Zscore$Area.Season <-  paste(db.TP.plot.Zscore$Area, db.TP.plot.Zscore$Season)

Zscoredb <- db.TP.plot.Zscore %>% group_by(Area.Season,Predictor) %>% dplyr::summarize(meanZ.val = mean(Value ,na.rm = T),  SDZ.val = sd(Value ,na.rm = T)) 
db.TP.plot.Zscore <- left_join(db.TP.plot.Zscore, Zscoredb)
db.TP.plot.Zscore$Z.Value <- (db.TP.plot.Zscore$Value - db.TP.plot.Zscore$meanZ.val) / db.TP.plot.Zscore$SDZ.val

ggplot(db.TP.plot.Zscore %>% filter(Predictor %in% c("Phe","TP.Chikaraishi", "TP.Nielsen"   ,  "TP.GluAla"   ,   "TP.AllTro_Phe" , "TP.All_no.norm")) , aes(Year, Z.Value,colour=Predictor ))+ geom_point()+ geom_line(linetype="dotted", size= 1)+facet_wrap(~Area.Season)+ geom_smooth(method = "loess", se = F, linetype="solid", size= 0.7)+theme(legend.position="bottom")+
  theme(legend.text = element_text(size=15))

db.TP2 <- db.TP.plot.Zscore   %>%  pivot_wider(  # pivot all columns except case_id (all the symptoms columns)
  id_cols = c(-SDZ.val,-meanZ.val, - Value),
  names_from = "Predictor",    # assign name for new column that holds the symptoms
  values_from = "Z.Value")
#save db
#write.xlsx(db.TP2, "db.TP.Zvalue.xlsx")


