library(tidyverse)
library(readxl)
library(scales)
library(ggmap)
library(googlesheets)
library(dplyr)
library(ggforce)

#funding <- read_excel("data/TBEP_Funding_Sources_2016-2018.xlsx")
funding <- gs_title('TBEP_Leveraging_Data') %>% 
           gs_read(ws='data')

#Renaming all In-Kind, Match Contributions to just Match
funding$Type[funding$Type=="In-Kind"] <- "Match"

funding$Source_Type <- paste(funding$Source, funding$Type)

View(funding)

tbsum <- funding %>%  
  group_by(Source_Type) %>%
#  filter(Year=='FY17-18') %>% 
  summarise(sum_funds = sum(Amount)) %>% 
  mutate(Group = factor(Source_Type),
         cumulative = cumsum(sum_funds),
         midpoint = cumulative - sum_funds /2,
         label = paste0(round(sum_funds / sum(sum_funds)*100, 1), "%"),
         cols = c("#ffc000", "#ffd34d", 
                  "#70ad47", "#98c879",
                  "#bc8fdd", "#7030a0", "#dfcaef", 
                  "#a5a5a5", "#ed7d31", "#f3a977",
                  "#4472c4", "#7e9ed6", 
                  "#c00000", "#ff0e0e"))

tbsum$Group <- factor(tbsum$Group, levels = c("Non-Profit Match", "Private Cash", "Private Match", 
                                              "City Cash", "City Match",
                                              "County Cash", "County Match",
                                              "Regional Cash", "Regional Match",
                                              "State Cash", "State Match", 
                                              "Federal Cash", "Federal Match", "Federal CWA320"))

or_cols <- c("#a5a5a5", "#ed7d31", "#f3a977", 
          "#ffc000", "#ffd34d", 
          "#70ad47", "#98c879", 
          "#4472c4", "#7e9ed6", 
          "#c00000", "#ff0e0e", 
          "#bc8fdd", "#dfcaef", "#7030a0")

tbsum

pie(tbsum$sum_funds,labels = paste(tbsum$label,"\n","($",tbsum$sum_funds,")"), 
    clockwise = TRUE, col = tbsum$cols, 
    main = "Current & Past 2 Fiscal Years", cex=0.5, radius = 0.9)
legend(1.1, 1, tbsum$Group, cex=0.5, fill=tbsum$cols)

pc <- ggplot(tbsum, aes(x=1, y = sum_funds, fill=Group)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = or_cols) +
  geom_text(aes(label = paste(tbsum$label,"\n","($",tbsum$sum_funds,")")), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Current & Past 2 Fiscal Years") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

pc #How to get labels on outside circle?

tbsum2 <- tbsum %>%
          mutate(end = 2 * pi * cumsum(sum_funds)/sum(sum_funds),
                 start = lag(end, default = 0),
                 middle = 0.5 * (start + end),
                 hjust = ifelse(middle > pi, 1, 0),
                 vjust = ifelse(middle <pi/2 | middle > 3 * pi/2, 0, 1))
##Close
rlabel = 0.95
pc2 <- ggplot(tbsum2) +
         geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                          start = start, end = end, fill = Group)) +
         geom_text(aes(x = rlabel * sin(middle), y = rlabel * cos(middle), 
                       label = paste(tbsum$label,"\n","($",tbsum$sum_funds,")"),
                       hjust = 0.5, vjust = 0.5)) +
         scale_fill_manual(values = or_cols) +
         coord_fixed() +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Current & Past 2 Fiscal Years") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
         scale_x_continuous(limits = c(-1, 1),  # Adjust so labels are not cut off
                            name = "", breaks = NULL, labels = NULL) +
         scale_y_continuous(limits = c(-1, 1),      # Adjust so labels are not cut off
                            name = "", breaks = NULL, labels = NULL)  

pc2
