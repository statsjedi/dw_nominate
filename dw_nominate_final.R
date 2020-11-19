library(tidyverse)
library(ggridges)
library(readxl)
library(patchwork)

#year=congress x 2 + 1787

dw_data <- 
  read_excel("HSall_members.xlsx") %>% 
  filter(complete.cases(nominate_dim1)==TRUE) %>%
  filter(congress>=56) %>% #focus congresses from 56 (1899) onwards
  select(congress, nominate_dim1, party_code, chamber) %>% 
  filter(party_code==100 | party_code==200) #focus on Democrats (100) and Republicans (200)

#================================================
#Make Ridgeline Plot

p.ridgeline <- 
  dw_data %>% 
  mutate(breaks=cut(congress, breaks=2, include.lowest=TRUE)) %>% #split into 2 columns
  ggplot(aes(x=nominate_dim1, y=as.factor(congress*2+1787), fill=stat(x)))+
  geom_density_ridges_gradient(scale=3.5, rel_min_height = 0.01)+
  scale_fill_distiller(palette = "RdBu")+ #red/blue divergine palette
  xlab("DW-NOMINATE Score")+
  ylab("Year")+
  theme_classic()+
  theme(legend.position = "off")+
  facet_wrap(~breaks, scales="free_y")+
  theme(strip.background=element_blank(), strip.text=element_blank())

p.ridgeline

ggsave("ridgeline.png", p.ridgeline, width = 6.5, height=6.5)

#============================================
#Plot summary statistics

#make a function to calculate the mean, median, and sd
mean_median_sd <- function(x){
  mean_x <- mean(x$nominate_dim1)
  median_x <- median(x$nominate_dim1)
  sd_x <- sd(x$nominate_dim1)
  results <- data.frame(congress=unique(x$congress), party_code=unique(x$party_code),
                        chamber=unique(x$chamber), mean=mean_x, median=median_x, sd=sd_x)
  return(results)
}

dw_congress_chamber_party <- 
  dw_data %>% 
  group_by(congress, chamber, party_code) %>% 
  group_split() %>% #split data into a list by the groupings
  map(mean_median_sd) %>% #calculate mean, median, sd
  bind_rows() #recombine list into a data frame


dw_congress_party <- 
  dw_data %>% 
  group_by(congress, party_code) %>% 
  group_split() %>% #split data into a list by the groupings
  map(mean_median_sd) %>% #calculate mean, median, sd
  bind_rows() %>%  #recombine list into a data frame
  select(-chamber) %>% 
  unique()

#calculate polarization by subtracting the means from one another
polarization <- 
  dw_congress_party %>% 
  select(congress, party_code, mean) %>% 
  pivot_wider(names_from=party_code, values_from=mean) %>% 
  mutate(polarization=`200`-`100`) 

#plot DW-NOMINATE by party and chamber
ggplot(dw_congress_chamber_party, aes(x=congress, y=mean, color=paste(chamber, party_code, sep=" "),
                                      ymin=mean-2*sd, ymax=mean+2*sd, 
                                      fill=paste(chamber, party_code, sep=" ")))+
  geom_ribbon(alpha=0.25)+
  geom_point()+
  geom_line()
#looks like the house and senate are pretty close, although the senate is a little more to the 
#center than the house for each party

#plot DW-NOMINATE by party
ggplot(dw_congress_party, aes(x=congress, y=mean, color=as.factor(party_code),
                              ymin=mean-2*sd, ymax=mean+2*sd, fill=as.factor(party_code)))+
  geom_ribbon(alpha=0.25)+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  xlab("Year")+
  ylab("DW-NOMINATE\nMean and 95% CI")+
  theme(legend.position = "off")
#not bad, but let's smooth it

#plot polarization between the means over time
p.polarization <- 
  ggplot(polarization, aes(x=congress*2 + 1787, y=polarization))+
  geom_point()+
  geom_smooth(se=FALSE, color="black")+
  xlab("Year")+
  ylab("Polarization")+
  scale_x_continuous(breaks=seq(1900, 2020, 20))+
  theme_bw()

p.polarization 

ggsave("polarization.png", p.polarization, width = 6.5, height=6.5, units="in")

#make a function that will smooth the mean, median, and sd by loess
dw_smooth <- function(x){
  smooth_mean <- fitted(loess(mean~congress, data=x))
  smooth_median <- fitted(loess(median~congress, data=x))
  smooth_sd <- fitted(loess(sd~congress, data=x))
  results <- data.frame(congress=x$congress, party_code=x$party_code, s_mean=smooth_mean,
                        s_median=smooth_median, s_sd=smooth_sd, mean=x$mean, median=x$median,
                        sd=x$sd)
  return(results)
}

dw_congress_party_smoothed <- 
  dw_congress_party %>% 
  group_by(party_code) %>% 
  group_split() %>% #split by above group into a list
  map(dw_smooth) %>% #apply the smoothing function
  bind_rows() #recombine list into data frame


#plot mean and 95% CI over time
p.ribbon <- 
  ggplot(dw_congress_party_smoothed, 
         aes(x=congress*2+1787, y=s_mean, color=as.factor(party_code),
             ymin=s_mean-2*s_sd, ymax=s_mean+2*s_sd, fill=as.factor(party_code)))+
  geom_ribbon(alpha=0.25)+
  geom_line()+
  #geom_point()+
  geom_point(aes(y=mean))+
  #geom_point(aes(y=mean+2*sd))+
  #geom_point(aes(y=mean-2*sd))+
  scale_color_manual(values=c("#67a9cf", "#ef8a62"))+
  scale_fill_manual(values=c("#67a9cf", "#ef8a62"))+
  scale_x_continuous(breaks=seq(1900, 2020, 20))+
  xlab("Year")+
  ylab("Mean Score and 95%\nConfidence Interval")+
  theme_bw()+
  theme(legend.position = "off")

p.ribbon

ggsave("ribbon.png", p.ribbon, width = 6.5, height=6.5, units="in")

#plot sd over time
p.sd <- 
  ggplot(dw_congress_party_smoothed,
         aes(x=congress*2+1787, y=sd, color=as.factor(party_code)))+
  geom_point()+
  geom_smooth(se=FALSE)+
  scale_color_manual(values=c("#67a9cf", "#ef8a62"))+
  scale_x_continuous(breaks=seq(1900, 2020, 20))+
  xlab("Year")+
  ylab("Score Standard Deviation")+
  theme_bw()+
  theme(legend.position = "off")

p.sd

ggsave("sd.png", p.sd, width = 6.5, height=6.5, units="in")

#plot median over time
ggplot(dw_congress_party_smoothed,
       aes(x=congress*2+1787, y=median, color=as.factor(party_code)))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("blue", "red"))+
  scale_x_continuous(breaks=seq(1900, 2020, 20))+
  xlab("Year")+
  ylab("DW-NOMINATE Median")+
  theme(legend.position = "off")

#==========================================
#put it all together

p.ridgeline
p.ribbon
p.polarization
p.sd

p.final <- (p.ridgeline | (p.ribbon / p.sd / p.polarization)) +
  plot_annotation(title = "Congressional DW-NOMINATE Scores, 1899-2019")

p.final

ggsave("final.png", p.final, width=9, height=8, units="in")
