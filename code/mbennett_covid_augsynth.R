#Clear memory
rm(list = ls())

#Clear the console
cat("\014")

#Turn off scientific notation (turn back on with 0)
options(scipen = 999)

library(dplyr)
library(tidyr)
library(firasans)
library(shades)
library(ggplot2)
library(scales)
library(hrbrthemes)
library(stringr)
library(augsynth)
library(xtable)

hrbrthemes::update_geom_font_defaults(family=font_fsm) # Set up theme for ggplot

set.seed(100)

#### URLs to download data
URLresidence = "https://raw.githubusercontent.com/maibennett/covid_augsynth/master/data/jorgeperezrojas_long_confirmados_comunas_interpolado.csv"
dirComuna = "https://raw.githubusercontent.com/maibennett/covid_augsynth/master/data/comuna_income.csv"
###

#You can include update to data (depending on where it's grabbing it from)
loadData = function(baseURL) {
  d = read.csv(baseURL,stringsAsFactors=FALSE) 
  return(d)
}

#### Load all data
ResidenceCases = loadData(URLresidence) #Daily cases by municipality (interpolated)
IncComuna = loadData(dirComuna) #Data for municipalities (population, income), according to CASEN 2017
####

ResidenceCases = merge(ResidenceCases,IncComuna, by.x = "codigo_comuna",
                       by.y = "comuna_code", all.x = TRUE) # Merge both datasets

ResidenceCases$fecha = as.Date(ResidenceCases$fecha,"%m/%d/%Y") #Format date

ResidenceCases$day = as.numeric(ResidenceCases$fecha) - 18336 #Create a day since the first day we have data for

##############################################
# Create categories for the different municipalities (quarantine and income)

# High income categories
# 0: Not high income
# 1: High income
# 3: Other

ResidenceCases$high_income=3

ResidenceCases$quarantine = 0
ResidenceCases$quarantine[ResidenceCases$comuna.x=="Santiago" & 
                            ResidenceCases$day>=10 &
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Santiago"] = 0
                          
ResidenceCases$quarantine[ResidenceCases$comuna.y=="Nunoa" & 
                            ResidenceCases$day>=10 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Nunoa"] = 1

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Independencia" & 
                            ((ResidenceCases$day>=10 & 
                            ResidenceCases$day<=18) | ResidenceCases$day>=39)] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Independencia"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Las Condes" & 
                            ResidenceCases$day>=10 &
                            ResidenceCases$day<=32] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Las Condes"] = 1

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Vitacura" & 
                            ResidenceCases$day>=10 &
                            ResidenceCases$day<=29] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Vitacura"] = 1

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Lo Barnechea" & 
                            ResidenceCases$day>=10 &
                            ResidenceCases$day<=29] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Lo Barnechea"] = 1

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Providencia" & 
                            ResidenceCases$day>=10 &
                            ResidenceCases$day<=29] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Providencia"] = 1

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Puente Alto" & 
                            ResidenceCases$day>=23 &
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Puente Alto"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="El Bosque" & 
                            ResidenceCases$day>=32 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="El Bosque"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="San Bernardo" & 
                            ResidenceCases$day>=32 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="San Bernardo"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Quinta Normal" & 
                            ResidenceCases$day>=39 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Quinta Normal"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Pedro Aguirre Cerda" & 
                            ResidenceCases$day>=39 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Pedro Aguirre Cerda"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="La Pintana" & 
                            ResidenceCases$day>=46 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="La Pintana"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.y=="San Ramon" & 
                            ResidenceCases$day>=46 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="San Ramon"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Estacion Central" & 
                            ResidenceCases$day>=46 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Estacion Central"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Cerrillos" & 
                            ResidenceCases$day>=51 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Cerrillos"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Quilicura" & 
                            ResidenceCases$day>=51 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Quilicura"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Recoleta" & 
                            ResidenceCases$day>=51 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Recoleta"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Tortel" & 
                            ResidenceCases$day>=0 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Tortel"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Puerto Williams" & 
                            ResidenceCases$day>=10 & 
                            ResidenceCases$day<=22] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Puerto Williams"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Temuco" & 
                            ResidenceCases$day>=13 & 
                            ResidenceCases$day<=46] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Temuco"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Padre Las Casas" & 
                            ResidenceCases$day>=13 & 
                            ResidenceCases$day<=32] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Padre Las Casas"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Chillan" & 
                            ResidenceCases$day>=16 & 
                            ResidenceCases$day<=38] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Chillan"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Chillan Viejo" & 
                            ResidenceCases$day>=16 & 
                            ResidenceCases$day<=38] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Chillan Viejo"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Osorno" & 
                            ResidenceCases$day>=16 & 
                            ResidenceCases$day<=46] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Osorno"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Punta Arenas" & 
                            ResidenceCases$day>=17 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Punta Arenas"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.x=="San Pedro de la Paz" & 
                            ResidenceCases$day>=22 & 
                            ResidenceCases$day<=32] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="San Pedro de la Paz"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Hualpen" & 
                            ResidenceCases$day>=22 & 
                            ResidenceCases$day<=32] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Hualpen"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Arica" & 
                            ResidenceCases$day>=32 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Arica"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Angol" & 
                            ResidenceCases$day>=46 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Angol"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Victoria" & 
                            ResidenceCases$day>=46 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Victoria"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Antofagasta" & 
                            ResidenceCases$day>=51 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Antofagasta"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Mejillones" & 
                            ResidenceCases$day>=51 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Mejillones"] = 2

#################################

ResidenceCases = ResidenceCases %>% arrange(region,comuna.x, fecha) #sort data by municipality and date

ResidenceCases$CumCases = ResidenceCases$positivos_acumulados
ResidenceCases$CumCases[ResidenceCases$CumCases=="-"] = "0"

ResidenceCases$CumCases = as.numeric(ResidenceCases$CumCases)

# Create new cases by substracting cummulative cases each day
ResidenceCases = ResidenceCases %>% mutate(
  NewCases=CumCases - lag(CumCases, default=0)
)

ResidenceCases$NewCases[ResidenceCases$NewCases<0] = 0

# Take out municipalities that entered quarantine towards the end on the period of analysis
# (to avoid very short post period analysis)
comunas_out = c("La Pintana","San Ramon","Estacion Central","Quinta Normal","Pedro Aguirre Cerda",
                "Mejillones","Antofagasta","Victoria","Angol","Recoleta","Cerrillos","Quilicura")

# Keep only the period of study
d = ResidenceCases[ResidenceCases$day<=54 & !is.na(ResidenceCases$comuna.y),]

# Keep only large municipalities
d = d[d$Pop>70000 & !(d$comuna.y %in% comunas_out),]

###################################################### CHARACTERISTICS

comunas_high = unique(d$comuna.y[d$high_income==1])
comunas_low = unique(d$comuna.y[d$high_income==0])
comunas_regions = unique(d$comuna.y[d$high_income==2])

characteristics = d[!duplicated(d$codigo_comuna),]
characteristics$group = NA
characteristics$group[characteristics$comuna.y %in% comunas_high] = 1
characteristics$group[characteristics$comuna.y %in% comunas_low] = 2
characteristics$group[characteristics$high_income==3 & characteristics$region=="Metropolitana"] = 3
characteristics$group[characteristics$high_income==3 & characteristics$region!="Metropolitana"] = 4

char = characteristics %>% group_by(group) %>% summarise(population = mean(Pop),income=mean(inc_pc_comuna),
                                                         poverty = mean(poor_comuna),fonasa = mean(fonasa_comuna))

char_table = t(as.matrix(char[!is.na(char$group),]))[-1,]

colnames(char_table) = c("High_income","Low_income","Donor_RM","Donor_not_RM")

char_latex = xtable(char_table, digits=3)

print(char_table)

############################################################## AUGSYNTH

#### Overall effect

d$comuna = factor(d$comuna.y)

# Add controls: total cumulative cases, population, income per capita in the municipality and population density (hab/km2)
ppool_syn = multisynth(NewCases ~ quarantine | CumCases + Pop + inc_pc_comuna + density,
                       comuna, day, d, fixedeff = T, progfunc="Ridge")

ppool_syn_sum = summary(ppool_syn)

plot(summary(ppool_syn),levels = "Average")

results1 = ppool_syn_sum$att[ppool_syn_sum$att$Level=="Average" & ppool_syn_sum$att$Time>=0 &
                              !is.na(ppool_syn_sum$att$Time),]

results1 = results1[results1$Time>=13,]

results_total = matrix(NA,nrow=(2*nrow(results1)+4),ncol=4)

for(j in 1:nrow(results1)){
  results_total[2*j-1,1] = as.character(results1$Time[j])
  results_total[2*j-1,2] = as.character(round(results1$Estimate[j],3))
  results_total[2*j,2] = as.character(paste0("(",round(results1$Std.Error[j],3),")"))
}

results_total[22,1] = "Scaled Imbalance"
results_total[22,2] = round(ppool_syn_sum$scaled_global_l2,3)
results_total[23,1] = "Num. leads"
results_total[23,2] = ppool_syn_sum$n_leads
results_total[24,1] = "Num. lags"
results_total[24,2] = ppool_syn_sum$n_lags

#### High income municipalities:

comunas_high = unique(d$comuna.y[d$high_income==1])
comunas_low = unique(d$comuna.y[d$high_income==0])
comunas_regions = unique(d$comuna.y[d$high_income==2])

# Include comunas that want to be left out as robustness check
comunas_robust_check = c()

d_high = d[!(d$comuna.y %in% c(comunas_low,comunas_regions, comunas_robust_check)),]

ppool_syn_high = multisynth(NewCases ~ quarantine | CumCases + Pop + inc_pc_comuna + density, 
                            comuna, day, d_high, fixedeff = T,progfunc="Ridge")

ppool_syn_high_sum = summary(ppool_syn_high)

plot(summary(ppool_syn_high),levels = "Average")

results2 = ppool_syn_high_sum$att[ppool_syn_high_sum$att$Level=="Average" & 
                                    ppool_syn_high_sum$att$Time>=0 &
                               !is.na(ppool_syn_high_sum$att$Time),]

results2 = results2[results2$Time>=13 & results2$Time<=22,]

for(j in 1:nrow(results1)){
  results_total[2*j-1,3] = as.character(round(results2$Estimate[j],3))
  results_total[2*j,3] = as.character(paste0("(",round(results2$Std.Error[j],3),")"))
}

results_total[22,3] = round(ppool_syn_high_sum$scaled_global_l2,3)
results_total[23,3] = ppool_syn_high_sum$n_leads
results_total[24,3] = ppool_syn_high_sum$n_lags

#### Low income municipalities

# Include comunas that want to be left out as robustness check
comunas_robust_check = c()

d_low = d[!(d$comuna.y %in% c(comunas_high,comunas_regions, comunas_robust_check)),]

ppool_syn_low = multisynth(NewCases ~ quarantine | CumCases + Pop + inc_pc_comuna + density, 
                           comuna, day, d_low, fixedeff = T,progfunc="Ridge")

ppool_syn_low_sum = summary(ppool_syn_low)

plot(summary(ppool_syn_low),levels = "Average")

results3 = ppool_syn_low_sum$att[ppool_syn_low_sum$att$Level=="Average" & 
                                    ppool_syn_low_sum$att$Time>=0 &
                                    !is.na(ppool_syn_low_sum$att$Time),]

results3 = results3[results3$Time>=13 & results3$Time<=22,]

for(j in 1:nrow(results1)){
  results_total[2*j-1,4] = as.character(round(results3$Estimate[j],3))
  results_total[2*j,4] = as.character(paste0("(",round(results3$Std.Error[j],3),")"))
}


results_total[22,4] = round(ppool_syn_low_sum$scaled_global_l2,3)
results_total[23,4] = ppool_syn_low_sum$n_leads
results_total[24,4] = ppool_syn_low_sum$n_lags

xtable(results_total)


#### Low income municipalities that entered quarantine early

comunas_low = unique(d$comuna.y[d$high_income==0])
comunas_stgo = comunas_low[!(comunas_low %in% c("Santiago","Independencia"))]
  
d_stgo = d[!(d$comuna.y %in% c(comunas_high,comunas_regions, comunas_stgo)),]

ppool_syn_stgo = multisynth(NewCases ~ quarantine | CumCases + Pop + inc_pc_comuna + density, 
                           comuna, day, d_stgo, progfunc="Ridge")

ppool_syn_stgo_sum = summary(ppool_syn_stgo)

plot(summary(ppool_syn_stgo),levels = "Average")


######################################################### GGPLOT

#### High income

synth =  ppool_syn_high_sum$att[ppool_syn_high_sum$att$Level=='Average',]
names(synth) = c("day","level","est","se")

synth = synth[!is.na(synth$day),]

ggplot(data = synth, aes(x=day,y=est)) +
  geom_ribbon(aes(ymin=est-1.645*se,ymax=est+1.645*se),alpha=0.2,fill="#2ca3c1") +
  geom_line(lwd=1.2, col="#2ca3c1") +
  geom_point(col="#2ca3c1")+
  geom_vline(xintercept = 0, color = alpha("#545454",0.5), lty=5,lwd=1.2) +
  geom_vline(xintercept = 1, color = "#545454", lty=3,lwd=1.2) +
  geom_vline(xintercept = 13, color = "#545454", alpha=0.3, lty=2, lwd = 1.2) +
  geom_hline(yintercept = 0, color="#545454") +
  
  annotate("rect",xmin=10,xmax=16,ymin=17.5,ymax=20.5,col="white",fill="white") +
  annotate("text",x=13,y=19,hjust=0.5,vjust=0.5,label="12-day mark", font=font_fsm) +
  
  annotate("rect",xmin=1.5,xmax=5.5,ymin=17.5,ymax=20.5,col="white",fill="white") +
  annotate("text",x=1.5,y=19,hjust=0,vjust=0.5,label="Start Q", font=font_fsm) +
  
  annotate("rect",xmin=-8.5,xmax=-0.5,ymin=17.5,ymax=20.5,col="white",fill="white") +
  annotate("text",x=-0.5,y=19,hjust=1,vjust=0.5,label="Q announced", font=font_fsm) +
  
  theme_bw()+
  theme_ipsum_fsc(plot_title_face = "bold") + #plain 
  xlab("Days since Quarantine (Q) Announcement") + ylab("Est. Difference in New Cases")+ #ggtitle("Estimated Effect of Quarantine for High Income Municipalities")+
  theme(plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.title.x = element_text(size=15,margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size=8, angle=0,vjust = 0.6),
        axis.title.y = element_text(size=15,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size=8),legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=12, color = "#6D6F73"),
        legend.background = element_rect(fill="white",colour ="white"),
        title = element_text(size=14, color = "#6D6F73")) + ylim(-62,20.5)



#### Low income

synth =  ppool_syn_low_sum$att[ppool_syn_low_sum$att$Level=='Average',]
names(synth) = c("day","level","est","se")

synth = synth[!is.na(synth$day),]

ggplot(data = synth, aes(x=day,y=est)) +
  geom_ribbon(aes(ymin=est-1.645*se,ymax=est+1.645*se),alpha=0.2,fill="#9acd32") +
  geom_line(lwd=1.2, col="#9acd32") +
  geom_point(col="#9acd32")+
  geom_vline(xintercept = 0, color = alpha("#545454",0.5), lty=5,lwd=1.2) +
  geom_vline(xintercept = 2, color = "#545454", lty=3,lwd=1.2) +
  geom_vline(xintercept = 14, color = "#545454", alpha=0.3, lty=2, lwd = 1.2) +
  geom_hline(yintercept = 0, color="#545454") +
  
  annotate("rect",xmin=11,xmax=17,ymin=21,ymax=24,col="white",fill="white") +
  annotate("text",x=14,y=22.5,hjust=0.5,vjust=0.5,label="12-day mark", font=font_fsm) +
  
  annotate("rect",xmin=2.1,xmax=8.1,ymin=21,ymax=24,col="white",fill="white") +
  annotate("text",x=2.3,y=22.5,hjust=0,vjust=0.5,label="Start Q", font=font_fsm) +
  
  annotate("rect",xmin=-8.5,xmax=-0.5,ymin=21,ymax=24,col="white",fill="white") +
  annotate("text",x=-0.5,y=22.5,hjust=1,vjust=0.5,label="Q announced", font=font_fsm) +
  
  theme_bw()+
  theme_ipsum_fsc() + 
  xlab("Days since Quarantine (Q) Announcement") + ylab("Est. Difference in New Cases")+#ggtitle("Estimated Effect of Quarantine for Lower Income Municipalities")+
  theme(plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.title.x = element_text(size=15,margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size=8, angle=0,vjust = 0.6),
        axis.title.y = element_text(size=15,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size=8),legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=12, color = "#6D6F73"),
        legend.background = element_rect(fill="white",colour ="white"),
        title = element_text(size=14, color = "#6D6F73"))



#### Low income early quarantine

synth =  ppool_syn_stgo_sum$att[ppool_syn_stgo_sum$att$Level=='Average',]
names(synth) = c("day","level","est","se")

synth = synth[!is.na(synth$day),]

ggplot(data = synth, aes(x=day,y=est)) +
  geom_ribbon(aes(ymin=est-1.645*se,ymax=est+1.645*se),alpha=0.2,fill="#f7e500") +
  geom_line(lwd=1.2, col="#f7e500") +
  geom_point(col="#f7e500")+
  geom_vline(xintercept = 0, color = alpha("#545454",0.5), lty=5,lwd=1.2) +
  geom_vline(xintercept = 1, color = "#545454", lty=3,lwd=1.2) +
  geom_vline(xintercept = 13, color = "#545454", alpha=0.3, lty=2, lwd = 1.2) +
  geom_hline(yintercept = 0, color="#545454") +
  
  annotate("rect",xmin=10,xmax=16,ymin=28.5,ymax=31.5,col="white",fill="white") +
  annotate("text",x=13,y=30,hjust=0.5,vjust=0.5,label="12-day mark", font=font_fsm) +
  
  annotate("rect",xmin=1.5,xmax=5.5,ymin=28.5,ymax=31.5,col="white",fill="white") +
  annotate("text",x=1.5,y=30,hjust=0,vjust=0.5,label="Start Q", font=font_fsm) +
  
  annotate("rect",xmin=-8.5,xmax=-0.5,ymin=28.5,ymax=31.5,col="white",fill="white") +
  annotate("text",x=-0.5,y=30,hjust=1,vjust=0.5,label="Q announced", font=font_fsm) +
  
  theme_bw()+
  theme_ipsum_fsc() + 
  xlab("Days since Quarantine (Q) Announcement") + ylab("Est. Difference in New Cases")+#ggtitle("Estimated Effect of Quarantine for Early Entrance, Lower Income Municipalities")+
  theme(plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.title.x = element_text(size=15,margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size=8, angle=0,vjust = 0.6),
        axis.title.y = element_text(size=15,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size=8),legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=12, color = "#6D6F73"),
        legend.background = element_rect(fill="white",colour ="white"),
        title = element_text(size=14, color = "#6D6F73")) #+ ylim(-12,23)


#### Overall effect

synth =  ppool_syn_sum$att[ppool_syn_sum$att$Level=='Average',]
names(synth) = c("day","level","est","se")

synth = synth[!is.na(synth$day),]

ggplot(data = synth, aes(x=day,y=est)) +
  geom_ribbon(aes(ymin=est-1.645*se,ymax=est+1.645*se),alpha=0.2,fill="#ff531f") +
  geom_line(lwd=1.2, col="#ff531f") +
  geom_point(col="#ff531f")+
  geom_vline(xintercept = 0, color = alpha("#545454",0.5), lty=5,lwd=1.2) +
  geom_vline(xintercept = 2, color = "#545454", lty=3,lwd=1.2) +
  geom_vline(xintercept = 14, color = "#545454", alpha=0.3, lty=2, lwd = 1.2) +
  geom_hline(yintercept = 0, color="#545454") +
  
  annotate("rect",xmin=11,xmax=17,ymin=21,ymax=24,col="white",fill="white") +
  annotate("text",x=14,y=22.5,hjust=0.5,vjust=0.5,label="12-day mark", font=font_fsm) +
  
  annotate("rect",xmin=2.1,xmax=8.1,ymin=21,ymax=24,col="white",fill="white") +
  annotate("text",x=2.3,y=22.5,hjust=0,vjust=0.5,label="Start Q", font=font_fsm) +
  
  annotate("rect",xmin=-8.5,xmax=-0.5,ymin=21,ymax=24,col="white",fill="white") +
  annotate("text",x=-0.5,y=22.5,hjust=1,vjust=0.5,label="Q announced", font=font_fsm) +
  
  theme_bw()+
  theme_ipsum_fsc() + 
  xlab("Days since Quarantine (Q) Announcement") + ylab("Est. Difference in New Cases")+#ggtitle("Estimated Effect of Quarantine for Lower Income Municipalities")+
  theme(plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(axis.title.x = element_text(size=15,margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size=8, angle=0,vjust = 0.6),
        axis.title.y = element_text(size=15,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size=8),legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=12, color = "#6D6F73"),
        legend.background = element_rect(fill="white",colour ="white"),
        title = element_text(size=14, color = "#6D6F73")) + ylim(-18,26)



############################################### ROBUSTNESS CHECK FOR BUFFER ZONES

#### High income:

comunas_high = unique(d$comuna.y[d$high_income==1])
comunas_low = unique(d$comuna.y[d$high_income==0])
comunas_regions = unique(d$comuna.y[d$high_income==2])

comunas_buffer = c("Penalolen","La Reina","Macul","Recoleta","Vina del Mar","Concon")

d_high = d[!(d$comuna.y %in% c(comunas_low,comunas_regions, comunas_buffer)),]

ppool_syn_high = multisynth(NewCases ~ quarantine | CumCases + Pop + inc_pc_comuna + density, 
                            comuna, day, d_high, fixedeff = T, progfunc="Ridge")

ppool_syn_high_sum = summary(ppool_syn_high)

plot(summary(ppool_syn_high),levels = "Average")

results1 = ppool_syn_high_sum$att[ppool_syn_high_sum$att$Level=="Average" & ppool_syn_high_sum$att$Time>=0 &
                               !is.na(ppool_syn_high_sum$att$Time),]

results1 = results1[results1$Time>=13 & results1$Time<=22,]

results_total = matrix(NA,nrow=(2*nrow(results1)+4),ncol=4)

for(j in 1:nrow(results1)){
  results_total[2*j-1,1] = as.character(results1$Time[j])
  results_total[2*j-1,2] = as.character(round(results1$Estimate[j],3))
  results_total[2*j,2] = as.character(paste0("(",round(results1$Std.Error[j],3),")"))
}

results_total[22,1] = "Scaled Imbalance"
results_total[22,2] = round(ppool_syn_high_sum$scaled_global_l2,3)
results_total[23,1] = "Num. leads"
results_total[23,2] = ppool_syn_high_sum$n_leads
results_total[24,1] = "Num. lags"
results_total[24,2] = ppool_syn_high_sum$n_lags


#### Low income:

comunas_buffer = c("Maipu","Lo Espejo","Recoleta","La Cisterna","La Pintana","La Florida")

d_low = d[!(d$comuna.y %in% c(comunas_high,comunas_regions, comunas_buffer)),]

ppool_syn_low = multisynth(NewCases ~ quarantine | CumCases + Pop + inc_pc_comuna + density, 
                           comuna, day, d_low, fixedeff = T, progfunc="Ridge")

ppool_syn_low_sum = summary(ppool_syn_low)

plot(summary(ppool_syn_low),levels = "Average")


results3 = ppool_syn_low_sum$att[ppool_syn_low_sum$att$Level=="Average" & 
                                   ppool_syn_low_sum$att$Time>=0 &
                                   !is.na(ppool_syn_low_sum$att$Time),]

results3 = results3[results3$Time>=13 & results3$Time<=22,]

for(j in 1:nrow(results1)){
  results_total[2*j-1,3] = as.character(round(results3$Estimate[j],3))
  results_total[2*j,3] = as.character(paste0("(",round(results3$Std.Error[j],3),")"))
}


results_total[22,3] = round(ppool_syn_low_sum$scaled_global_l2,3)
results_total[23,3] = ppool_syn_low_sum$n_leads
results_total[24,3] = ppool_syn_low_sum$n_lags

xtable(results_total)
