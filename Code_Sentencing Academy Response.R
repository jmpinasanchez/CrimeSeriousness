
################################################################################
###############CHANGES IN SEVERITY##############################################
################################################################################

library(tseries) 
library(ggplot2)
library(zoo)      #This is to use as.yearmon


#Data ##########################################################################
data = read.csv("data_plots_R.csv", header = TRUE)


#Graphs ########################################################################

#Setting the date
table(data$Year, useNA="ifany")
data$date = as.Date(paste("15-", "06-", data$Year, sep = ""), format = "%d-%m-%Y")

#Expressing change in relative terms
data$indictable_rlength = data$indictable_length / data$indictable_length[1]
data$either_way_rlength = data$either_way_length / data$either_way_length[1]
data$summary_nonmotoring_rlength = data$summary_nonmotoring_length / data$summary_nonmotoring_length[1]
data$summary_motoring_rlength = data$summary_motoring_length / data$summary_motoring_length[1]
data$intent_rlength = data$intent_length / data$intent_length[1]
data$GBH_rlength = data$GBH_length / data$GBH_length[1]
data$common_rlength = data$common_length / data$common_length[1]
data$manslaughter_rlength = data$manslaughter_length / data$manslaughter_length[1]
data$robbery_rlength = data$robbery_length / data$robbery_length[1]
data$supplyA_rlength = data$supplyA_length / data$supplyA_length[1]
data$supplyB_rlength = data$supplyB_length / data$supplyB_length[1]
data$sexual_assault_rlength = data$sexual_assault_length / data$sexual_assault_length[1]
data$rape_rlength = data$rape_length / data$rape_length[1]
data$drugs_rsev = data$drugs_sev / data$drugs_sev[1]
data$drugs_rser = data$drugs_ser / data$drugs_ser[1]
data$damage_rsev = data$damage_sev / data$damage_sev[1]
data$damage_rser = data$damage_ser / data$damage_ser[1]
data$sex_rsev = data$sex_sev / data$sex_sev[1]
data$sex_rser = data$sex_ser / data$sex_ser[1]


#Severity index for rape, GBH, robbery, and manslaughter########################

#Calculating the severity index
data$rape_sev = data$rape_cust * data$rape_length
data$intent_sev = data$intent_cust * data$intent_length
data$GBH_sev = data$GBH_cust * data$GBH_length
data$robbery_sev = data$robbery_cust * data$robbery_length
data$manslaughter_sev = data$manslaughter_cust * data$manslaughter_length
#Now in relative terms
data$rape_rsev = data$rape_sev / data$rape_sev[1]
data$intent_rsev = data$intent_sev / data$intent_sev[1]
data$GBH_rsev = data$GBH_sev / data$GBH_sev[1]
data$robbery_rsev = data$robbery_sev / data$robbery_sev[1]
data$manslaughter_rsev = data$manslaughter_sev / data$manslaughter_sev[1]

#Relative change in severity index by offence type
ggplot(data, aes(x=date)) +
  geom_line(aes(y=manslaughter_rsev, color="Manslaughter"), size=1) + 
  geom_line(aes(y=intent_rsev, color="GBH with intent"), size=1) + 
# geom_line(aes(y=GBH_rsev, color="GBH"), size=1) + 
  geom_line(aes(y=robbery_rsev, color="Robbery"), size=1) + 
  geom_line(aes(y=rape_rsev, color="Rape"), size=1) +   
  ylab("Imprisonment use relative to 2004") + xlab("Year") + 
  labs(color="Offence type") + theme_bw() + 
  scale_color_manual(
    values = c("Manslaughter"="#440154FF", "GBH with intent"="#3B528BFF", 
               "Robbery"="#5DC863FF", "Rape"="#FDE725FF"), 
    breaks = c("Manslaughter", "GBH with intent", "Common assault",  
               "Robbery", "Supply Class A", "Supply Class B", "Sexual assault",
               "Rape")) +  # Specify legend entry order  
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=10))  



#Seriousness and severity trends################################################ 

#Drugs
ggplot(data, aes(x=date)) +
  geom_line(aes(y=drugs_rser, color="Crime seriousness"), size=1) + 
  geom_line(aes(y=drugs_rsev, color="Sentence severity"), size=1) + 
  ylab("Imprisonment use relative to 2004") + xlab("Year") + 
  labs(color="") + theme_bw() + 
  scale_color_manual(
    values = c("Crime seriousness"="#440154FF", 
               "Sentence severity"="#FDE725FF"),  # Specify colors
    breaks = c("Crime seriousness", 
               "Sentence severity")) +  # Specify legend entry order
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=10))  

#Criminal Damage
ggplot(data, aes(x=date)) +
  geom_line(aes(y=damage_rser, color="Crime seriousness"), size=1) + 
  geom_line(aes(y=damage_rsev, color="Sentence severity"), size=1) + 
  ylab("Imprisonment use relative to 2004") + xlab("Year") + 
  labs(color="") + theme_bw() + 
  scale_color_manual(
    values = c("Crime seriousness"="#440154FF", 
               "Sentence severity"="#FDE725FF"),  # Specify colors
    breaks = c("Crime seriousness", 
               "Sentence severity")) +  # Specify legend entry order
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=10))  

#Sex
ggplot(data, aes(x=date)) +
  geom_line(aes(y=sex_rser, color="Crime seriousness"), size=1) + 
  geom_line(aes(y=sex_rsev, color="Sentence severity"), size=1) + 
  ylab("Imprisonment use relative to 2004") + xlab("Year") + 
  labs(color="") + theme_bw() + 
  scale_color_manual(
    values = c("Crime seriousness"="#440154FF", 
               "Sentence severity"="#FDE725FF"),  # Specify colors
    breaks = c("Crime seriousness", 
               "Sentence severity")) +  # Specify legend entry order
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=10))  

