# packages ----------------------------------------------------------------

library(tidyverse)
library(hrbrthemes)
extrafont::loadfonts(device = "win", quiet = T)
library(geofacet)
library(ggtext)
library(paletteer)

# set dates ---------------------------------------------------------------

date.start.lockdown <- as.Date("2020-03-14")
date.opening.shops <- as.Date("2020-04-14")

df_dates <- tibble(date.start.lockdown=date.start.lockdown,
                   date.opening.shops=date.opening.shops) %>% 
  pivot_longer(cols=everything(),
               names_to = "event",
               values_to="date")


# import csv --------------------------------------------------------------
file_link <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

df_global <- readr::read_csv(file=file_link, col_types = "ccccDdddddd")

df_AT <- df_global %>% 
  filter(country_region_code=="AT") %>% 
  mutate(sub_region_1=case_when(is.na(sub_region_1) ~ "State level",
                   TRUE ~ as.character(sub_region_1)))

df_AT_long <- df_AT %>% 
  pivot_longer(cols=contains("baseline"),
                      names_to="type",
                      values_to="value") %>% 
  mutate(week.day=lubridate::wday(date, label=T), .after=date) %>% 
  mutate(sub_region_1=forcats::as_factor(sub_region_1))

unique(df_AT_long$type)

df_AT_long <- df_AT_long %>% 
  mutate(place.description=case_when(str_detect(type, "retail") ~ "‘places like restaurants, cafes, shopping centers, theme parks, museums, libraries, and movie theaters.’",
                                     str_detect(type, "grocery") ~ "’places like grocery markets, food warehouses, farmers markets, specialty food shops, drug stores, and pharmacies.’",
                                     str_detect(type, "park") ~ "‘places like local parks, national parks, public beaches, marinas, dog parks, plazas, and public gardens.’",
                                     str_detect(type, "transit") ~ "‘places like public transport hubs such as subway, bus, and train stations.’",
                                     str_detect(type, "work") ~ "‘places of work’",
                                     str_detect(type, "residential") ~ "‘places of residence’",
                                     TRUE ~ as.character("missing")))


# replicate reports -------------------------------------------------------
#reports start with feb 29

df_AT_long %>% 
  ggplot()+
  labs(title=glue::glue("Mobility data from {min(df_AT_long$date)} to {max(df_AT_long$date)}"))+
  geom_line(aes(x=date,
                y=value))+
  geom_vline(data=df_dates,
             aes(xintercept = date),
             color="orange")+
  facet_grid(rows=vars(sub_region_1),
             cols=vars(type),
             switch = "y",
             labeller=as_labeller(function(x) str_remove_all(x, regex("_percent.*$")) %>% 
                                    str_replace_all(., "_", " ")))+
  hrbrthemes::theme_ipsum_rc()+
  theme(strip.placement = "outside",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_blank(),
        strip.text.y.left = element_text(angle=0,
                                         margin = margin(t=0, unit="cm"),
                                         vjust=1,
                                         hjust=1))


# Overlap ----------------------------------------------------------------

AUT_states_grid <- data.frame(
  code = c("4", "3", "9", "8", "7", "5", "6", "1", "2"),
  name_de = c("Oberösterreich", "Niederösterreich", "Wien", "Vorarlberg", "Tirol", "Salzburg", "Steiermark", "Burgenland", "Kärnten"),
  name_en=c("Upper Austria", "Lower Austria", "Vienna", "Vorarlberg", "Tyrol", "Salzburg", "Styria", "Burgenland", "Carinthia"),
  row = c(1, 1, 1, 2, 2, 2, 2, 2, 3),
  col = c(3, 4, 5, 1, 2, 3, 4, 5, 4))
  # code = c("AT31", "AT12", "AT13", "AT34", "AT33", "AT32", "AT22", "AT11", "AT21"),

# geofacet::grid_submit(x=AUT_states_grid, name="AUT_states_grid", desc="Grid of 9 Austrian Federal States")

txt_explain <- "Changes for each day are compared to a baseline value for that day of the week: The baseline is the median value, for the corresponding day of the week, during the 5-week period Jan 3–Feb 6, 2020. For details see https://www.google.com/covid19/mobility/"
date_shutdown <- as.Date("2020-03-16")

df_AT_long_type <- df_AT_long %>% 
  filter(str_detect(type, "parks")) %>% 
  filter(!str_detect(sub_region_1, "State")) 
  
df_AT_long_type %>% 
  ggplot()+
  labs(title=paste("Changes in Mobility during Covid-19 crisis:", 
                    str_to_upper(str_remove_all(unique(df_AT_long_type$type), regex("_percent.*$")))),
       subtitle=str_wrap(txt_explain, 150),
       caption=paste0("data: Google Mobility Report"))+
  geom_line(data=df_AT_long %>% 
              filter(str_detect(type, "parks")) %>% 
              filter(!str_detect(sub_region_1, "State")) %>% 
              rename(all_regions=sub_region_1),
            aes(x=date,
                y=value,
                group=all_regions),
            color="grey70",
            show.legend=F,
            stat="identity") +
  geom_line(aes(x=date,
                y=value,
                color=sub_region_1),
            show.legend=F,
            size=1)+
  # geom_vline(aes(xintercept=date_shutdown,
  #            # name="shutdown",
  #            # show.legend = T,
  #            linetype="shutdown"),
  #            colour="red",
  #            )+
  geom_vline(data=df_dates,
             aes(xintercept=date,
                 linetype=event),
             color="red")+
  scale_linetype_manual(values=c(date.opening.shops="dotted",
                                 date.start.lockdown="solid"),
                        labels=c(date.opening.shops="shops partly open",
                                 date.start.lockdown="start lockdown"))+
  theme_ipsum_rc()+
  scale_x_date(labels = scales::label_date_short(),
               expand=expansion(mult=c(0, 0.15)),
               breaks = c(seq.Date(min(df_AT_long$date), 
                                   max(df_AT_long$date), 
                                   by="2 weeks"))) +
  scale_y_continuous(labels=scales::label_percent(accuracy = 1,
                                                  scale=1),
                     limits=c(-100, 100))+
  scale_color_paletteer_d("ggsci::default_uchicago")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.title.y=element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text.y = element_text(angle = 0, vjust = 1),
    legend.position = "bottom",
    legend.justification = "right",
    legend.key.size=unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    # plot.title = element_text(size = 12),
    panel.spacing = unit(0, "cm"),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = c(0, 1), color = "grey30"),
    # plot.margin = margin(0, r = 0.2, unit = "cm")
  )+
  facet_geo(~sub_region_1, grid = AUT_states_grid)+
  guides(linetype = guide_legend(override.aes = list(colour = "red",
                                                     direction="vertical"),
                                 reverse = T),
         colour = "none")

  
ggsave("plot.png", type="cairo",
       unit="cm",
       scale=2,
       width=16,
       height=10)


# weekdays ----------------------------------------------------------------

df_AT_long %>% 
  mutate(lockdown.indicator=case_when(date>=date.start.lockdown ~ "lockdown",
                                      date<date.start.lockdown ~ "before lockdown")) %>% 
  filter(str_detect(type, "parks")) %>% 
  filter(!str_detect(sub_region_1, "State")) %>% {
  ggplot(., aes(x=sub_region_1,
                y=value)) +
      geom_jitter(aes(color=lockdown.indicator),
                  height = 0.5)+
      scale_color_manual(values=c("lockdown"="red",
                                  "before lockdown"="grey"))+
      scale_y_continuous(limits=c(-100, 100))+
      facet_wrap(vars(week.day))+
      theme_ipsum_rc()+
      theme(legend.position = "bottom",
            legend.justification="right",
            legend.title = element_blank(),
            legend.direction = "horizontal")+
      coord_flip()
  }



# boxplot before after ----------------------------------------------------

unique(df_AT_long$type)

df_AT_long <- df_AT_long %>% 
  mutate(lockdown.indicator=case_when(date>=date.start.lockdown ~ "lockdown",
                                      date<date.start.lockdown ~ "before lockdown")) %>% 
  mutate(weekend.indicator=case_when(week.day %in% c("Sat", "Sun") ~ "weekend",
                                     TRUE ~ as.character("not weekend"))) #%>% 
  
fn_map <- function(df, my_type) {
  
  df %>% 
  filter(str_detect(type, my_type)) %>% 
  filter(!str_detect(sub_region_1, "State")) %>% {
    ggplot(., aes(x=lockdown.indicator,
                  y=value))+
      labs(title=paste("Changes in Mobility during Covid-19 crisis:", 
                       str_to_upper(str_remove_all(unique(.$type), regex("_percent.*$")))),
           # subtitle=str_wrap(txt_explain, 150),
           subtitle=str_c(str_wrap(txt_explain, 100), 
                          "\n\n", 
                          str_wrap(paste(str_to_sentence(str_remove_all(unique(.$type), regex("_percent.*$"))),  
                                   "are defined as", 
                                   unique(.$place.description)), 100)),
           caption=paste0("data: Google Mobility Report"))+
      gghalves::geom_half_boxplot(side="l",
                                  aes(color=NULL),
                                  outlier.shape = NA,
                                  # nudge=0.2,
                                  show.legend = F)+
      gghalves::geom_half_point_panel(aes(color=weekend.indicator),
                                side="r",
                                range_scale = 0.75,
                                size=0.4,
                                # nudge=0.2,
                                transformation=position_jitter(width=0.8))+
      scale_x_discrete(labels=c("before lockdown"="before", "lockdown"="during"))+
      scale_color_paletteer_d("ggsci::default_jama")+
      scale_y_continuous(limits=c(-100, 100),
                         labels = scales::label_percent(scale = 1),
                         minor_breaks = NULL,
                         position="left")+
      facet_geo(~sub_region_1, grid = AUT_states_grid,
                labeller=as_labeller(function(x) str_remove_all(x, regex("_percent.*$")) %>%
                                       str_replace_all(., "_", " ")))+
      theme_ipsum_rc()+
      theme(axis.title.x=element_blank(),
            axis.title.y.right = element_text(),
            panel.grid.major.x = element_blank(),
            plot.title.position = "plot",
            legend.title = element_blank(),
            legend.position = "top",
            legend.justification = "left",
            legend.direction = "horizontal")+
      guides(color=guide_legend(override.aes=list(size=6)))
    
  }
}


pl_list <- unique(df_AT_long$type) %>% 
  map(., ~fn_map(df=df_AT_long, my_type=.x))


