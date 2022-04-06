# Seafood Production 
# 2.a. Harvest Value
# Version 1.0. March 1, 2022

### packages ####
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(scales)
library(tidytext)
library(glue)
library(patchwork)
library(showtext)
library(ragg)
library(here)

font_add_google("Roboto", "roboto")                
font_add_google("Mitr", "mitr")
font_add_google("Khula", "khula")
font_add_google("Share Tech Mono", "techmono")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

### Load data

production <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/Processed_Data/MaineDMR_Modern_Landings_Data_2021-11-16_clean.csv", 
                       stringsAsFactors = FALSE)


### Cleaning and merging - production already in long format

#renaming year to Year
production <- production %>%
  rename(Year = year,
         Dollars = value) 

#keeping 2010-2020
production <- filter(production, Year >= 2010)

###

bck_color <- "#000000"

production_sum <- production %>%
  group_by(Year, NOAA.category) %>%
  summarise(total = sum(Dollars)) %>%
  ungroup()


lines_label <- production_sum %>%
  filter(Year == max(Year)) %>%
  arrange(total) %>%
  mutate(cum = cumsum(total),                                  #cumulative value for 2020
         posy = lag(cum, default = 0) + total / 2) %>%         #location of left end of the lines going from graph to labels
  bind_cols(posyend = seq(6e7, 7.5e8, 7.5e8 / 7))                  #location of the right end of the lines going from graph to labels          


year_list <- tibble(year = seq(2010, 2020, 1))                 #numbers making up the x-axis at the end

fish_order <- production_sum %>%
  filter(Year == max(Year)) %>%
  arrange(desc(total)) %>%
  pull(NOAA.category)                                          #ordering the line graph by total value


axis_p1 <- tibble(x = c(2010, 2014, 2017),                  #years less than the years the values are associated with so they can bee seen. 
                  xend = c(2012, 2016, 2019),               #years I'm showing in white near the peaks in the graph
                  y = c(5.3e8, 7.3e8, 6.75e8),              #locations for those numbers
                  yend = c(5.3e8, 7.3e8, 6.75e8),           #ditto (I think?)  
                  yend_lab = c("530 M", "730 M", "675 M"))  #I created this so we could have simple labels - call this as the "label"    



color_fish <- c()
color_fish[fish_order] <- colorRampPalette(c("#233749", "#e37a55"))(7)  


production_species_plt <- production_sum %>%
  mutate(NOAA.category = fct_relevel(NOAA.category, fish_order)) %>%          #making NOAA category a factor and reordering it(?)
  ggplot() +
  geom_segment(data = axis_p1, aes(x = x, xend = xend, y = y, yend = yend), 
               color = "white", linetype = "13", size = 0.3, inherit.aes = FALSE) +                                  #size of graph
  geom_text(data = axis_p1, aes(x = x, y = yend, label = yend_lab), 
            color = "white", hjust = 0, nudge_y = 2e7, size = 3, family = "roboto", inherit.aes = FALSE) +           #labels on peaks  
  geom_text(data = year_list, aes(x = year, y = -9e6, label = year), 
            color = "white", hjust = 1.25, vjust = 1, angle = 45,  
            size = 3.5, family = "roboto", inherit.aes = TRUE) +                                                     #new x-axis       
  geom_area(aes(Year, total, fill = NOAA.category), color = "grey1") +                                              
  geom_segment(data = lines_label, aes(x = 2020, xend = 2025, y = posy, yend = posyend, color = NOAA.category)) +    #left-hand lines
  geom_segment(data = lines_label, aes(x = 2028, xend = 2033, y = posyend, yend = posyend, color = NOAA.category)) + #right-hand lines
  geom_label(data = lines_label, aes(x = 2025, y = posyend, label = NOAA.category, color = NOAA.category), 
             fill = bck_color, size = 3.5, family = "khula", nudge_y = 1.5e7, label.size = 0, 
             hjust = 0, fontface = "bold", inherit.aes = FALSE) +                                                    #spp names near lines
  geom_label(data = lines_label, aes(x = 2025, y = posyend, label = glue("{comma(total)} Dollars"), color = NOAA.category), 
             fill = bck_color, size = 3.5, family = "khula", nudge_y = -1.5e7, label.size = 0, 
             hjust = 0, fontface = "bold", inherit.aes = FALSE) +                                                    #values near lines
  annotate("text", x = 2010, y = 8.5e8, label = "(A) Value by Species Category (2010-2020)", 
           family = "khula", size = 5, color = "white", hjust = 0) +                                                 #label
  annotate("text", x = 2033, y = 8.5e8, label = "(B) Total Value Landed by Region (2020)", 
           family = "khula", size = 5, color = "white", hjust = 0) +                                                 #label
  scale_fill_manual(values = color_fish) +                                                                          
  scale_color_manual(values = color_fish) +
  scale_x_continuous(limits = c(2010, 2050)) +                                                                       #full size of plot  
  guides(fill = "none", color = "none") +                                         
  theme_void() +                                                                                                     #this and above remove                                                                                                                       built-in lines/axes
  theme(plot.background = element_rect(fill = bck_color, color = NA))  

prod_reorder <- production %>%
  filter(Year == 2020) %>%
  group_by(region, NOAA.category) %>%
  summarise(total = sum(Dollars)) %>%
  ungroup() %>%
  mutate(tot_order = ifelse(NOAA.category == "Other", 0, total),       #move other category to the end - not sure if it is relevant here
         NOAA.category = fct_relevel(NOAA.category, rev(fish_order)),  #making NOAA.category a factor and giving it an order (see line 119)
         Entity_wt = reorder_within(region, tot_order, NOAA.category))  #reorder state by total (with other = 0) within NOAA.category groups

prod_colors <- tibble(Color = colorRampPalette(c("#474747", "#ebebeb"))(4),  #selecting colors - took some googling to get right. (#)  indicates number of bins
                      region = c("Downeast", "Midcoast", "Southern", 
                                 "Unidentified"))             #names to go with each color


color_within <- prod_reorder %>%
  left_join(prod_colors)                                                     #adding color info to prod_reorder


color_cty <- c() 
color_cty[color_within$Entity_wt] <- color_within$Color                      #not sure about these, but identifying the colors to use


axis_p2 <- tibble(x = c(1, 2, 3, 4, 5, 6, 7),                          #size of x-axis
                  xend = rep(7.7, 7),                                        #end size
                  y = seq(0e8, 4.5e8, 0.75e8),                              #y-axis
                  yend = seq(0e8, 4.5e8, 0.75e8),                           #end of y-axis
                  yend_lab = c("0", "75 M", "150 M", 
                               "225 M", "300 M", "375 M", "450 M"))          #simple labels

production_cty_plt <- prod_reorder %>%
  ggplot(aes(as.numeric(NOAA.category), total, fill = Entity_wt)) +                               #plot                       
  geom_segment(data = axis_p2, aes(x = c(1, 7, 7, 7, 7, 7, 7), xend = xend, y = y, yend = yend), 
               color = "white", linetype = "13", size = 0.3, inherit.aes = FALSE) +               #full size
  geom_text(data = axis_p2, aes(x = xend, y = yend, label = yend_lab), 
            color = "white", nudge_x = 0.2, size = 3.5, family = "roboto", inherit.aes = FALSE) + #axis numbering
  geom_col(color = "grey30", width = 0.6, size = 0.2) +                                           #creating columns
  coord_flip() +                                                                                  #rotating
  guides(fill = "none") +                                                                         #removing original background and axis
  scale_fill_manual(values = color_cty) +                                                         #adding in custom color
  theme_void()                                                                                    #removing background (?)

### Legend

legend_plt <- prod_colors %>%
  bind_cols(y = seq(4 , 1, -1)) %>%                                             #adding column of #s to the color info
  ggplot()+
  geom_rect(aes(xmin = 1, xmax = 2, ymin = y, ymax = y + 0.8, fill = Color)) +  #Size of legend 
  geom_text(aes(x = 1.5, y = y + 0.4, label = region), 
            size = 3, family = "roboto", color = "black") +                     #labeling and text size
  scale_fill_identity() +                                                       #coloring
  theme_void()                                                                  #removing background

### Final plot

final <- production_species_plt +                                                                     #line graph 
  inset_element(production_cty_plt, 0.555, 0.04, 1, 0.94) +                                          #bar graph and location coords
  inset_element(legend_plt, 0.9, 0.1, 0.98, 0.4) +                                                    #legend and location coords
  plot_annotation(
    title = "Seafood Value in Maine (2010-2020)",
    theme = theme(
      plot.background = element_rect(fill = bck_color, color = NA),
      plot.margin = margin(10, 5, 5, 0),
      plot.title = element_text(family = "mitr", size = 25, color = "white", 
                                hjust = 0.5, vjust = 0.2, margin = margin(5, 0, 15, 0)), 
      #plot.caption = element_text(family = "techmono", size = 9, color = "white", hjust = 0.95)     #labels and final sizing
    )
  )

ragg::agg_png(here::here("render", paste0("2.a.Total_Seafood_Value_ME_2010_2020", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
              res = 320, width = 18, height = 8, units = "in")                                       #making png and telling it where to go

final                                                                                                #print final graph

dev.off()                                                                                            #render graph (?)
#RUN THIS WHOLE CHUNK AT ONCE OR IT WON'T WORK!!!

# Save figure 

ggsave("2.a.Total_Seafood_Value_ME_2010_2020.pdf", width = 15, height = 9)
