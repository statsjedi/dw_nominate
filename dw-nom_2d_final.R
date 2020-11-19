library(tidyverse)
library(readxl)
library(gifski)

#year=congress x 2 + 1787

dw_data <- 
  read_excel("HSall_members.xlsx") %>% 
  filter(complete.cases(nominate_dim1)==TRUE & complete.cases(nominate_dim2)==TRUE) %>%
  filter(congress>=56) %>% #focus congresses from 56 (1899) onwards
  select(congress, nominate_dim1, nominate_dim2, party_code, chamber) %>% 
  filter(chamber != "President") %>% #limit to Congress
  filter(party_code==100 | party_code==200) %>%  #focus on Democrats (100) and Republicans (200)
  mutate(year=congress*2+1787)

dwplot_2d <- function(x){
  my_year <- unique(x$year)
  
  p.temp <- 
    ggplot(x, aes(x=nominate_dim1, y=nominate_dim2))+
    geom_density_2d_filled(bins=9)+
    geom_hline(yintercept = 0, color="red")+
    geom_vline(xintercept = 0, color="red")+
    theme(legend.position = "off")+
    scale_x_continuous(limits=c(-1, 1), expand=c(0,0))+
    scale_y_continuous(limits=c(-1, 1), expand=c(0, 0))+
    labs(title="US Congressional DW-NOMINATE Scores 1899-2019",
         subtitle=paste("[", my_year, "]", sep=""))+
    coord_equal()+
    xlab("(Liberal) Economic Axis (Conservative)")+
    ylab("(Liberal) Social Axis (Conservative)")
    
  temp_congress <- unique(x$congress)
  if(temp_congress<100){
    temp_congress <- paste("0", temp_congress, sep="")
  }
  
  savename <- paste("animation/plot_", temp_congress, ".png", sep="")
  
  ggsave(savename, p.temp, width=6.5, height=6.5, units="in")
}

dw_data %>% 
  group_by(congress) %>% 
  group_split() %>% 
  map(dwplot_2d)

png_files <- list.files("animation/", pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "dw_animation.gif", width = 1000, height = 1000, delay = 0.5)

