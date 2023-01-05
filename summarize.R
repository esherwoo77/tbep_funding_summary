library(tidyverse)
library(readxl)
library(scales)
library(ggmap)
library(googlesheets4)
library(dplyr)
library(ggforce)
library(plotly)

#funding <- read_excel("data/TBEP_Funding_Sources_2016-2018.xlsx")
gs4_deauth()
url <- "https://docs.google.com/spreadsheets/d/1zZMlQ1TIR8jJKz5ge7NbQVOlx_m2G5l4jv5gTbX-SFc"
funding <- read_sheet(url)

#Drop NAs
funding <- funding[complete.cases(funding[ , 2]),]

#Renaming all In-Kind, Leveraged, Match Contributions to just Match
funding$Type[funding$Type %in% c("In-Kind", "Leveraged")] <- "Match"
funding$Source[funding$Source %in% c("State-PA")] <- "State-Other"
funding$Source[funding$Source %in% c("Regional-CA")] <- "State-Other"
funding$Source[funding$Source %in% c("Non-profit")] <- "Non-Profit"

funding$Source_Type <- paste(funding$Source, funding$Type)
funding$Source_Type[funding$Source_Type %in% c("Non-Profit Cash")] <- "Private Cash"
funding$Source_Type[funding$Source_Type %in% c("Non-Profit Match")] <- "Private Match"

View(funding)

#funding <- funding %>% 
#           mutate(Amount2 = as.numeric(Amount))

years <- c("FY2023", "FY2022", "FY2021")
list_grp <- unique(funding$Source_Type)
view(list_grp)

tbsum <- funding %>%  
  group_by(Source_Type) %>%
  filter(Year %in% years) %>%
  summarise(sum_funds = sum(na.omit(Amount)), .groups = 'drop_last') %>% 
  mutate(Group = factor(Source_Type),
         cumulative = cumsum(sum_funds),
         midpoint = cumulative - sum_funds /2,
         label = paste0(round(sum_funds / sum(sum_funds)*100, 1), "%"),
         cols = c("#ffd34d", "#ffc000", 
                  "#70ad47", "#98c879",
                  "#bc8fdd", "#7030a0", #"#dfcaef", #Federal Match
                  "#ed7d31", "#f3a977",
                  "#4472c4", "#7e9ed6", 
                  #"#F4CCCC",             #state-Other cash 
                  "#c00000", "#ff0e0e"
                  )
                  )

write_excel_csv(tbsum,"./data/TBEP_Funding_Sources_2021-2023.csv")

#Attempt to automate pie charts below ...

wpsum <- funding %>% 
  group_by(Funding_Entity) %>% 
  filter(Year %in% years, (Type == "Cash" | Type == "CWA320")) %>% 
  filter(Program == "TBEP Work Plan") %>% 
  summarise(sum_funds = sum(na.omit(Amount)), .groups = 'drop_last') %>% 
  mutate(Group = factor(Funding_Entity),
         cumulative = cumsum(sum_funds),
         midpoint = cumulative - sum_funds /2,
         label = paste0(round(sum_funds / sum(sum_funds)*100, 1), "%"))
         
  
# "#a5a5a5", <-"Non-Profit Match" color insert, if needed
# "#757575", <-"Non-Profit Cash" color insert, if needed
# "#ffc000", <-"Non-Profit Match" color insert, if needed
# "#dfcaef", <-"Federal Match" color insert, if needed
 

#tbsum$Group <- factor(tbsum$Group, levels = c("Non-Profit Match", "Private Cash", "Private Match", 
#                                              "City Cash", "City Match",
#                                              "County Cash", "County Match",
#                                              "Regional Cash", "Regional Match",
#                                              "State Cash", "State Match", 
#                                              "Federal Cash", "Federal CWA320"))
#"Non-Profit Cash", insert, if needed above
#"Federal Match", insert, if needed above

or_cols <- tbsum$cols

# "#ffc000", <-"Non-Profit Match" color insert, if needed
# "#dfcaef", <-"Federal Match" color insert, if needed

tbsum

pc <- ggplot(tbsum, aes(x=1, y = sum_funds, fill=Group)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = or_cols) +
  geom_text(aes(label = paste(label,"\n","($",sum_funds,")")), 
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
#"Non-Profit Cash", "Federal Match",
customOrder <- c("Private Cash", "Private Match", 
                  "City Cash", "City Match",
                  "County Cash", "County Match",
                  "Regional Cash", "Regional Match",
                  "State-Other Cash", "State Cash", "State Match", 
                  "Federal Cash", "Federal CWA320") 
customOrder <- c(rev(customOrder[3:length(customOrder)]),customOrder[2],customOrder[1])
or_cols2 <- c(rev(or_cols[3:length(or_cols)]),or_cols[2],or_cols[1])
tbsum3 <- tbsum %>% slice(match(customOrder, tbsum$Source_Type))
tbsum3$Source_Type <- factor(tbsum3$Source_Type, levels = tbsum3[["Source_Type"]])

pie(tbsum3$sum_funds,labels = paste(tbsum3$label,"\n","($",tbsum3$sum_funds,")"), 
    clockwise = TRUE, col = tbsum3$cols, 
    main = "Current & Past 2 Fiscal Years", cex=0.5, radius = 0.9)
legend(1.1, 1, tbsum3$Group, cex=0.4, fill=tbsum3$cols)


pc3 <- plot_ly(tbsum3, labels= ~Source_Type, 
               values= ~sum_funds, sort = FALSE) %>%   
               add_pie(textposition = 'inside', 
                       textinfo = 'percent+value', 
                       insidetextfont = list(color = '#FFFFFF'),
                       marker = list(colors = ~cols,
                                     line = list(color = '#FFFFFF', width = 1))) %>%   
               #The 'pull' attribute can also be used to create space between the sectors
        layout(title = 'Fiscal Years 2021-2023', showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pc3

