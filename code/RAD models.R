
###################################################
#### RAD MODELS FOR ABUNDANCE DATA (ARKAUTE) #####
###################################################

library(vegan)
library(readxl)
library(tidyverse)
library(ggpubr)
#library(gambin)

# Function to normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

source("code/first_script.R")



data=as.data.frame(flora)
rm(flora)
data_original=data
data=data[,c(1:6)]
#data <- mutate(data, abundance = ifelse(abundance < 1, 1, abundance)) #####
data <- mutate(data, abundance = abundance + 1) ##### Sum 1 to all abundances (if we round first, some species with very low abundances may reflect abundance=0)
data$abundance = round(data$abundance, digits=0) # radfit() does not like decimals to estimate AIC/BIC

#radfit(data$abundance) # Lognormal fit best in terms of AIC/BIC (after Mandelbrot, but this one is more difficult to interpret)

data_c = data %>% filter(treatment == "c") # filters control sites
data_p = data %>% filter(treatment == "p") # filters perturbed sites
data_w = data %>% filter(treatment == "w") # filters warmed sites
data_wp = data %>% filter(treatment == "wp") # filters warmed+perturbed sites
data$treatment=as.factor(data$treatment)

#radfit(data_c$abundance) # Preemption best
#radfit(data_p$abundance) # Lognormal best
#radfit(data_w$abundance) # Preemption best
#radfit(data_wp$abundance) # Lognormal best

# Normalizing does not(?) affect results
#data_c$abundance = normalize(data_c$abundance)
#data_p$abundance = normalize(data_p$abundance)
#data_w$abundance = normalize(data_w$abundance)
#data_wp$abundance = normalize(data_wp$abundance)


######### CONTROL PLOTS
rad_c = matrix(nrow=length(unique(data_c$sampling))*length(unique(data_c$plot)), ncol=10) # samplings
colnames(rad_c)=c("treatment","sampling","plot","Species_richness","Mean_abundance","Zipf_p1","Zipf_gamma","Preemp_alpha","Lognormal_mu","Lognormal_sigma") #,"Gambin_alpha")
rad_c=as.data.frame(rad_c)

cont=0 # Loop counter
sortplots_c <- sort(unique(data_c$plot)) # Sort plot numbers within treatments (necessary for the loop)
for (i in 0:(length(unique(data_c$sampling))-1)){
  for (j in 1:c(length(unique(data_c$plot)))){
    sub <- filter (data_c, sampling==i & plot==sortplots_c[j]) #sub-dataset with sampling i and column of abundances
    radfit1 <- rad.zipf(sub$abundance)
    radfit2 <- rad.preempt(sub$abundance)
    radfit3 <- rad.lognormal(sub$abundance)
#    gambin <- fit_abundances(sub$abundance)
    
    cont=cont+1
    rad_c[cont,1]="control"
    rad_c[cont,2]=as.numeric(i)
    rad_c[cont,3]=sortplots_c[j]
    rad_c[cont,4]=length(unique(sub$species)) # Species richness
    rad_c[cont,5]=mean(sub$abundance) # Mean biomass
    rad_c[cont,6]=radfit1$coefficients[1] # Zipf model (p1 parameter)
    rad_c[cont,7]=radfit1$coefficients[2] # Zipf model (gamma parameter)
    rad_c[cont,8]=radfit2$coefficients[1] # Preemption model (alpha parameter)
    rad_c[cont,9]=radfit3$coefficients[1] # Lognormal model (mu parameter)
    rad_c[cont,10]=radfit3$coefficients[2] # Lognormal model (sigma parameter)
#    rad_c[cont,11]=gambin$alpha
    }
  }

######### PERTURBED PLOTS
rad_p = matrix(nrow=length(unique(data_p$sampling))*length(unique(data_p$plot)), ncol=10) # samplings
colnames(rad_p)=c("treatment","sampling","plot","Species_richness","Mean_abundance","Zipf_p1","Zipf_gamma","Preemp_alpha","Lognormal_mu","Lognormal_sigma") #,"Gambin_alpha")
rad_p=as.data.frame(rad_p)

cont=0 # Loop counter
sortplots_p <- sort(unique(data_p$plot)) # Sort plot numbers within treatments (necessary for the loop)
for (i in 0:(length(unique(data_p$sampling))-1)){
#  if(i==1)
#  {
#    i=i+1
#  }
  for (j in 1:c(length(unique(data_p$plot)))){
    sub <- filter (data_p, sampling==i & plot==sortplots_p[j]) #sub-dataset with sampling i and column of abundances
    radfit1 <- rad.zipf(sub$abundance)
    radfit2 <- rad.preempt(sub$abundance)
    radfit3 <- rad.lognormal(sub$abundance)
#    gambin <- fit_abundances(sub$abundance, subsample = 0)
    
    cont=cont+1
    rad_p[cont,1]="perturbed"
    rad_p[cont,2]=as.numeric(i)
    rad_p[cont,3]=sortplots_p[j]
    rad_p[cont,4]=length(unique(sub$species)) # Species richness
    rad_p[cont,5]=mean(sub$abundance) # Mean biomass
    rad_p[cont,6]=radfit1$coefficients[1] # Zipf model (p1 parameter)
    rad_p[cont,7]=radfit1$coefficients[2] # Zipf model (gamma parameter)
    rad_p[cont,8]=radfit2$coefficients[1] # Preemption model (alpha parameter)
    rad_p[cont,9]=radfit3$coefficients[1] # Lognormal model (mu parameter)
    rad_p[cont,10]=radfit3$coefficients[2] # Lognormal model (sigma parameter)
#    rad_p[cont,11]=gambin$alpha
  }
}

######### WARMED PLOTS
rad_w = matrix(nrow=length(unique(data_w$sampling))*length(unique(data_w$plot)), ncol=10) # samplings
colnames(rad_w)=c("treatment","sampling","plot","Species_richness","Mean_abundance","Zipf_p1","Zipf_gamma","Preemp_alpha","Lognormal_mu","Lognormal_sigma") #,"Gambin_alpha")
rad_w=as.data.frame(rad_w)

cont=0 # Loop counter
sortplots_w <- sort(unique(data_w$plot)) # Sort plot numbers within treatments (necessary for the loop)
for (i in 0:(length(unique(data_w$sampling))-1)){
  for (j in 1:c(length(unique(data_w$plot)))){
    sub <- filter (data_w, sampling==i & plot==sortplots_w[j]) #sub-dataset with sampling i and column of abundances
    radfit1 <- rad.zipf(sub$abundance)
    radfit2 <- rad.preempt(sub$abundance)
    radfit3 <- rad.lognormal(sub$abundance)
#    gambin <- fit_abundances(sub$abundance)
    
    cont=cont+1
    rad_w[cont,1]="warmed"
    rad_w[cont,2]=as.numeric(i)
    rad_w[cont,3]=sortplots_w[j]
    rad_w[cont,4]=length(unique(sub$species)) # Species richness
    rad_w[cont,5]=mean(sub$abundance) # Mean biomass
    rad_w[cont,6]=radfit1$coefficients[1] # Zipf model (p1 parameter)
    rad_w[cont,7]=radfit1$coefficients[2] # Zipf model (gamma parameter)
    rad_w[cont,8]=radfit2$coefficients[1] # Preemption model (alpha parameter)
    rad_w[cont,9]=radfit3$coefficients[1] # Lognormal model (mu parameter)
    rad_w[cont,10]=radfit3$coefficients[2] # Lognormal model (sigma parameter)
#    rad_w[cont,11]=gambin$alpha
  }
}

######### WARMED & PERTURBED PLOTS
rad_wp = matrix(nrow=length(unique(data_wp$sampling))*length(unique(data_wp$plot)), ncol=10) # samplings
colnames(rad_wp)=c("treatment","sampling","plot","Species_richness","Mean_abundance","Zipf_p1","Zipf_gamma","Preemp_alpha","Lognormal_mu","Lognormal_sigma") #,"Gambin_alpha")
rad_wp=as.data.frame(rad_wp)

cont=0 # Loop counter
sortplots_wp <- sort(unique(data_wp$plot)) # Sort plot numbers within treatments (necessary for the loop)
for (i in 0:(length(unique(data_wp$sampling))-1)){
#  if(i==1)
#  {
#    i=i+1
#  }
  for (j in 1:c(length(unique(data_wp$plot)))){
    sub <- filter (data_wp, sampling==i & plot==sortplots_wp[j]) #sub-dataset with sampling i and column of abundances
    radfit1 <- rad.zipf(sub$abundance)
    radfit2 <- rad.preempt(sub$abundance)
    radfit3 <- rad.lognormal(sub$abundance)
#    gambin <- fit_abundances(sub$abundance)
    
    cont=cont+1
    rad_wp[cont,1]="warmed&perturbed"
    rad_wp[cont,2]=as.numeric(i)
    rad_wp[cont,3]=sortplots_wp[j]
    rad_wp[cont,4]=length(unique(sub$species)) # Species richness
    rad_wp[cont,5]=mean(sub$abundance) # Mean biomass
    rad_wp[cont,6]=radfit1$coefficients[1] # Zipf model (p1 parameter)
    rad_wp[cont,7]=radfit1$coefficients[2] # Zipf model (gamma parameter)
    rad_wp[cont,8]=radfit2$coefficients[1] # Preemption model (alpha parameter)
    rad_wp[cont,9]=radfit3$coefficients[1] # Lognormal model (mu parameter)
    rad_wp[cont,10]=radfit3$coefficients[2] # Lognormal model (sigma parameter)
#    rad_wp[cont,11]=gambin$alpha
  }
}


##########  ALL TREATMENTS TOGETHER
rad_all = rbind(rad_c,rad_p,rad_w,rad_wp)
#rad_all = filter(rad_all, sampling > 1 & sampling < 11) # Exclude sampling 0, 1 & 11
#rad_all_original = rad_all


######### Add LOG RESPONSE RATIOS of mean species richness and abundance
# SPECIES RICHNESS
SR0 = rad_all[rad_all$sampling==0,]$Species_richness # Species richness values at the beggining (REFERENCE)
SR0_c=SR0[1:4]
SR0_p=SR0[5:8]
SR0_w=SR0[9:12]
SR0_wp=SR0[13:16]
SR0_c = rep(SR0_c,length(unique(data_c$sampling)))
SR0_p = rep(SR0_p,length(unique(data_p$sampling)))
SR0_w = rep(SR0_w,length(unique(data_w$sampling)))
SR0_wp = rep(SR0_wp,length(unique(data_wp$sampling)))
Species_richness_0 = c(SR0_c, SR0_p, SR0_w, SR0_wp) 
rad_all = cbind(rad_all, Species_richness_0)
rad_all <- rad_all %>% group_by (treatment, sampling) %>% mutate(logRR_Species_richness = log(Species_richness/Species_richness_0))
rad_all = as.data.frame(rad_all)

# ABUNDANCE
Ab0 = rad_all[rad_all$sampling==0,]$Mean_abundance # Species richness values at the beggining (REFERENCE)
Ab0_c=Ab0[1:4]
Ab0_p=Ab0[5:8]
Ab0_w=Ab0[9:12]
Ab0_wp=Ab0[13:16]
Ab0_c = rep(Ab0_c,length(unique(data_c$sampling)))
Ab0_p = rep(Ab0_p,length(unique(data_p$sampling)))
Ab0_w = rep(Ab0_w,length(unique(data_w$sampling)))
Ab0_wp = rep(Ab0_wp,length(unique(data_wp$sampling)))
Abundance_0 = c(Ab0_c, Ab0_p, Ab0_w, Ab0_wp)
rad_all = cbind(rad_all, Abundance_0)
rad_all <- rad_all %>% group_by (treatment, sampling) %>% mutate(logRR_Abundance = log(Mean_abundance/Abundance_0))
rad_all = as.data.frame(rad_all)


######### COEFFICIENT OF VARIATION (CV = sd/mean)  & LOG RESPONSE RATIO of CV #########

##### SPECIES RICHNESS
CV <- rad_all %>% group_by (sampling, treatment) %>% summarize(Mean_Species_richness = mean(Species_richness),
      SD_Species_richness = sd(Species_richness), CV_Species_richness = sd(Species_richness)/mean(Species_richness), n=n())
CV = as.data.frame(CV)
ggplot(CV, aes(group = treatment, x=treatment, y=CV_Species_richness, fill=treatment)) + geom_boxplot()
ggplot(CV, aes(group = treatment, x=treatment, y=CV_Species_richness, fill=treatment)) + geom_point() + facet_wrap(~sampling, scale="free") + ylim(0,1)
ggplot(CV, aes(group = sampling, x=sampling, y=CV_Species_richness, fill=treatment)) + geom_point() + facet_wrap(~treatment, scale="free") + ylim(0,1)
## Add CV to rad_all (I'm sure there's a better way with dplyr)
CV_all=rbind(CV,CV,CV,CV)
#CV_all=CV_all[order(a$treatment),]
CV_all=CV_all[with(CV_all, order(treatment, sampling)),]
rad_all=cbind(rad_all,CV_all)
rad_all=rad_all[,-c(15,16,20)]

# Add LOG RESPONSE RATIO of CV_Species_richness
CV0 = rad_all[rad_all$sampling==0,]$CV_Species_richness # Species richness values at the beggining (REFERENCE)
CV0_c=CV0[1:4]
CV0_p=CV0[5:8]
CV0_w=CV0[9:12]
CV0_wp=CV0[13:16]
#length(unique(data_c$sampling)) # Number of sampling times per treatment (control, in this case). Needed to know how many times to repeat the CV0 data per treatment
CV0_c = rep(CV0_c,length(unique(data_c$sampling)))
CV0_p = rep(CV0_p,length(unique(data_p$sampling)))
CV0_w = rep(CV0_w,length(unique(data_w$sampling)))
CV0_wp = rep(CV0_wp,length(unique(data_wp$sampling)))
CV_Species_richness_0 = c(CV0_c, CV0_p, CV0_w, CV0_wp) 
rad_all = cbind(rad_all, CV_Species_richness_0)
rad_all <- rad_all %>% group_by (treatment, sampling) %>% mutate(logRR_CV_Species_richness = log(CV_Species_richness/CV_Species_richness_0))
rad_all = as.data.frame(rad_all)

##### ABUNDANCE
CV <- rad_all %>% group_by (sampling, treatment) %>% summarize(SD_Mean_abundance = sd(Mean_abundance), CV_Mean_abundance = sd(Mean_abundance)/mean(Mean_abundance), n=n())
CV = as.data.frame(CV)
ggplot(CV, aes(group = treatment, x=treatment, y=CV_Mean_abundance, fill=treatment)) + geom_boxplot()
ggplot(CV, aes(group = treatment, x=treatment, y=CV_Mean_abundance, fill=treatment)) + geom_point() + facet_wrap(~sampling, scale="free") + ylim(0,1)
ggplot(CV, aes(group = sampling, x=sampling, y=CV_Mean_abundance, fill=treatment)) + geom_point() + facet_wrap(~treatment, scale="free") + ylim(0,1)
## Add CV to rad_all (I'm sure there's a better way with dplyr)
CV_all=rbind(CV,CV,CV,CV)
#CV_all=CV_all[order(a$treatment),]
CV_all=CV_all[with(CV_all, order(treatment, sampling)),]
rad_all=cbind(rad_all,CV_all)
rad_all=rad_all[,-c(20,21,24)]
# Add LOG RESPONSE RATIO of CV_Mean_abundance
CV0 = rad_all[rad_all$sampling==0,]$CV_Mean_abundance # Species richness values at the beggining (REFERENCE)
CV0_c=CV0[1:4]
CV0_p=CV0[5:8]
CV0_w=CV0[9:12]
CV0_wp=CV0[13:16]
CV0_c = rep(CV0_c,length(unique(data_c$sampling)))
CV0_p = rep(CV0_p,length(unique(data_p$sampling)))
CV0_w = rep(CV0_w,length(unique(data_w$sampling)))
CV0_wp = rep(CV0_wp,length(unique(data_wp$sampling)))
CV_Mean_abundance_0 = c(CV0_c, CV0_p, CV0_w, CV0_wp) 
rad_all = cbind(rad_all, CV_Mean_abundance_0)
rad_all <- rad_all %>% group_by (treatment, sampling) %>% mutate(logRR_CV_Mean_abundance = log(CV_Mean_abundance/CV_Mean_abundance_0))
rad_all = as.data.frame(rad_all)

################################
################ Treatment pairs
rad_all_original = rad_all
# Control Vs Perturbed
rad_c = rad_all %>% filter(treatment == "control")
rad_p = rad_all %>% filter(treatment == "perturbed")
rad_w = rad_all %>% filter(treatment == "warmed")
rad_wp = rad_all %>% filter(treatment == "warmed&perturbed")
rad_c_p = rbind(rad_c,rad_p)
rad_c_p$sampling=as.factor(rad_c_p$sampling)
# Control Vs Warmed
rad_c_w = rbind(rad_c,rad_w)
rad_c_w$sampling=as.factor(rad_c_w$sampling)
# Perturbed Vs Warmed+Perturbed
rad_p_wp = rbind(rad_p,rad_wp)
rad_p_wp$sampling=as.factor(rad_p_wp$sampling)
# Control Vs Warmed+Perturbed
rad_c_wp = rbind(rad_c,rad_wp)
rad_c_wp$sampling=as.factor(rad_c_wp$sampling)


############################################################
############### PLOTTING ##############################
############################################################
ggplot(rad_all, aes(group = sampling, x=sampling, y=Lognormal_mu, fill=treatment)) + geom_boxplot() + facet_wrap(~treatment, scale="free") + ylim(0,4)
ggplot(rad_all, aes(group = sampling, x=sampling, y=Species_richness, fill=treatment)) + geom_boxplot() + facet_wrap(~treatment, scale="free") + ylim(0,30)
ggplot(rad_all, aes(group = sampling, x=sampling, y=logRR_Species_richness, fill=treatment)) + geom_boxplot() + facet_wrap(~treatment, scale="free") + ylim(-2.5,0.75)
ggplot(rad_all, aes(group = sampling, x=sampling, y=logRR_CV_Species_richness, fill=treatment)) + geom_point() + facet_wrap(~treatment, scale="free") + ylim(-1.5,1.5)
ggplot(rad_all, aes(group = sampling, x=sampling, y=Mean_abundance, fill=treatment)) + geom_boxplot() + facet_wrap(~treatment, scale="free") + ylim(0,55)
ggplot(rad_all, aes(group = sampling, x=sampling, y=logRR_Abundance, fill=treatment)) + geom_boxplot() + facet_wrap(~treatment, scale="free") + ylim(-3.5,1.5)
ggplot(rad_all, aes(group = sampling, x=sampling, y=logRR_CV_Mean_abundance, fill=treatment)) + geom_point() + facet_wrap(~treatment, scale="free") + ylim(-2.8,0.5)


# Statistical tests (http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/)
#my_comparisons <- list( c("control", "perturbed"), c("control", "warmed"), c("control", "warmed&perturbed"), c("perturbed","warmed"), c("perturbed","warmed&perturbed"),c("warmed","warmed&perturbed") )
my_comparisons <- list( c("control", "perturbed"), c("control", "warmed"), c("control", "warmed&perturbed"), c("perturbed","warmed&perturbed"))
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "ns"))

#################
# All samplings together
ggplot(rad_all, aes(x=treatment, y=Lognormal_mu, fill=treatment)) + geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, symnum.args = symnum.args) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 2)     # Add global p-value
ggplot(rad_all, aes(x=treatment, y=Species_richness, fill=treatment)) + geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, symnum.args = symnum.args) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0)     # Add global p-value
ggplot(rad_all, aes(x=treatment, y=Mean_abundance, fill=treatment)) + geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, symnum.args = symnum.args) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1)     # Add global p-value

#################
# Per sampling
ggplot(rad_all, aes(x=treatment, y=Lognormal_mu, fill=treatment)) + geom_boxplot() + facet_wrap(~sampling, scale="free") + ylim(-6,2) +
  stat_compare_means(comparisons = my_comparisons, symnum.args = symnum.args) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1)     # Add global p-value
ggplot(rad_all, aes(x=treatment, y=Species_richness, fill=treatment)) + geom_boxplot() + facet_wrap(~sampling, scale="free") + ylim(0,30) +
  stat_compare_means(comparisons = my_comparisons, symnum.args = symnum.args) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1)     # Add global p-value
ggplot(rad_all, aes(x=treatment, y=Mean_abundance, fill=treatment)) + geom_boxplot() + facet_wrap(~sampling, scale="free") +
  stat_compare_means(comparisons = my_comparisons, symnum.args = symnum.args) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1)     # Add global p-value


#################
# TREATMENT PAIRS COMPARISONS
#my_comparisons <- list( c("0","1"), c("1","2"), c("2","3"), c("3","4"), c("4","5"), c("5","6"), c("6","7"), c("7","8"), c("8","9"), c("9","10"), c("10","11"))
my_comparisons <- list( c("0","0"), c("1","1"), c("2","2"), c("3","3"), c("4","4"), c("5","5"), c("6","6"), c("7","7"), c("8","8"), c("9","9"), c("10","10"), c("11","11"))
rad_c_p$sampling = as.factor(rad_c_p$sampling)
ggplot(rad_c_p, aes(x=sampling, y=Mean_abundance, fill=treatment)) + geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, symnum.args = symnum.args) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1)  



#############################################################
###### EMPIRICAL CUMMULATIVE DISTRIBUTION FUNCTIONS (ECDFs) #########
# CHECK MATTHEWS & WHITTAKER, JAE 2015

rad_all = filter(rad_all, sampling > 1 & sampling < 11) # Exclude sampling 0, 1 & 11

# Firts, Rank-Abundance Dsitributions (RADs) for Mean abundance values per treatment
par(mfrow=c(1,1))
plot(rev(sort(log(rad_c$Mean_abundance))), lwd=2, type="l", col="darkgreen", xlab="Species rank", ylab="Abundance (log scale)", main="RADs", ylim=c(0,4.2))
lines(rev(sort(log(rad_p$Mean_abundance))), lwd=2, col="brown", xlab="", ylab="")
lines(rev(sort(log(rad_w$Mean_abundance))), lwd=2, col="red", xlab="", ylab="")
lines(rev(sort(log(rad_wp$Mean_abundance))), lwd=2, col="blue", xlab="", ylab="")
legend(26, 4, legend=c("Control", "Perturbed", "Warmed", "Warmed + Perturbed"),col=c("darkgreen", "brown", "red", "blue"), lty=1:2, lwd=2, cex=1)

# 4 panel plot with ECDFs
par(mfrow=c(2,2))
plot(ecdf(log(rad_c$Mean_abundance)), verticals = TRUE, do.points = FALSE, lwd =2, col="darkgreen", xlab="Abundance (log scale)", ylab="Proportion", main="CONTROL PLOTS")
plot(ecdf(log(rad_p$Mean_abundance)), verticals = TRUE, do.points = FALSE, lwd =2, col="brown", xlab="Abundance (log scale)", ylab="Proportion", main="PERTURBED PLOTS")
plot(ecdf(log(rad_w$Mean_abundance)), verticals = TRUE, do.points = FALSE, lwd =2, col="red", xlab="Abundance (log scale)", ylab="Proportion", main="WARMED PLOTS")
plot(ecdf(log(rad_wp$Mean_abundance)), verticals = TRUE, do.points = FALSE, lwd =2, col="blue", xlab="Abundance (log scale)", ylab="Proportion", main="WARMED PLOTS")

# Overlapping ECDFs
par(mfrow=c(1,1))
plot(ecdf(log(rad_c$Mean_abundance)), verticals = TRUE, do.points = FALSE, lwd =2, col="darkgreen", xlab="Abundance (log scale)", ylab="Proportion", main="ECDFs", xlim=c(0,5))
lines(ecdf(log(rad_p$Mean_abundance)), verticals = TRUE, do.points = FALSE, lwd =2, col="brown", xlab="Abundance (log scale)", ylab="Proportion")
lines(ecdf(log(rad_w$Mean_abundance)), verticals = TRUE, do.points = FALSE, lwd =2, col="red", xlab="Abundance (log scale)", ylab="Proportion")
lines(ecdf(log(rad_wp$Mean_abundance)), verticals = TRUE, do.points = FALSE, lwd =2, col="blue", xlab="Abundance (log scale)", ylab="Proportion")
legend(0, 0.9, legend=c("Control", "Perturbed", "Warmed", "Warmed + Perturbed"),col=c("darkgreen", "brown", "red", "blue"), lty=1:2, lwd=2, cex=1)
