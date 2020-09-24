# loading neccessary packages
require(tidyverse)
require(readxl)
require(lubridate)
require(patchwork)
require(plyr)
# set ggplot theme
theme_set(
          theme_classic() + 
    theme(axis.line = element_line(size = .1, linetype = 1, colour = "white")) +
    theme(panel.background = element_rect(size = 1, linetype = 1, colour = rgb(0.3, 0.3, 0.3, 0.7))) +
    theme(strip.background = element_rect(size = 0, linetype = 1, colour = rgb(0.3, 0.3, 0.3)))
)


ggplot2::update_geom_defaults("point", list(shape = 21, fill = "gray", 
                                            colour = "black", alpha = 1, size = 2))
scale_colour_discrete <- function(...) {
    scale_colour_brewer(..., palette = "Dark2")
}
ggplot2::theme_set(ggplot2::theme_light() + ggplot2::theme(axis.line = ggplot2::element_blank(), 
                                                           panel.background = ggplot2::element_rect(fill = "transparent"), 
                                                           plot.background = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5, 
                                                                                                                                          vjust = -10)))

# custom function of boxplot
data_summary <- function(x) {
   m <- mean(x)
   ymin <- m-sd(x)
   ymax <- m+sd(x)
   return(c(y=m,ymin=ymin,ymax=ymax))
}


# load data
load("alldf1_5.RData")

# summarize  the IP_date
table(alldf1$IP_date < ymd(20200414))
# 
# FALSE  TRUE 
#    10   267 


# Fig 3
world_map <- map_data("world")
p1 <- ggplot(world_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill="lightgray", colour = "white", size = 0.2) +
    geom_point(data = alldf1 %>% arrange(-IP_R),
               aes(x = long, y = lat, group = key, fill = log10(IP_R)),
               # fill = rgb(0.3, 0.5, 0.8),
               size = 2.4,
               # colour = "transparent",
               colour = rgb(0.3, 0.3, 0.3),
               shape = 21,
               alpha = 0.9) +
    theme_void() +
    labs(fill = NULL) +
    # geom_text(data = tibble(long = -159, lat = 34), aes(x = long, y = lat), label = expression(~R[I]), group = 1) +
    # geom_text(data = tibble(long = 52, lat = -53), aes(x = long, y = lat),
              # label = "Transmission rate at point of inflection", group = 1) +
    # geom_text(data = tibble(long = 52, lat = -53), aes(x = long, y = lat), label = "Transmission rate at point of inflection", group = 1) +
    # scale_size_continuous(breaks = c(10, 20, 50, 100, 200, 400)) +
    geom_text(data = tibble(long = 81, lat = -48), aes(x = long, y = lat),
              label = "Transmission rate at point of inflection", group = 1) +
              # label = "Change of air temperature in July (°C)", group = 1) +
    # scale_size_continuous(range = c(2, 5)) +
    # scale_fill_manual(values = c(rgb(0.5, 0.8, 0.3), rgb(0.8, 0.5, 0.3))) +
    scale_fill_gradient2(low = "blue", midpoint = 0, high = "red",
                         breaks = -2:3,
                         # labels = -2:3,
                         labels = scales::math_format(10^.x),
                         limit = c(-2, 3)) +
    # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                  # labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  # limit = c(0.01, 1000)) +
    # theme(legend.position = c(0.64, 0.18),
    theme(legend.position = c(0.69, 0.18),
          legend.direction = "horizontal",
          legend.key.width = unit(1.384,"cm"),
          legend.background = element_rect(fill = NA, colour = NA)) +
    guides(size = guide_legend(nrow=1, byrow=TRUE))
p2 <- ggplot(alldf1, aes(IP_R)) +
    geom_histogram(fill = rgb(0.3, 0.5, 0.8), colour = "black", size = 0.1, bins = 30, alpha = 0.4) +
    scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  limit = c(0.01, 1000)) +
    coord_flip() +
    # labs(x = expression("Transmission rate at IP (cases"~day^{-1}~M^{-1}~")"), 
    # labs(x = expression(~R[I]), 
    labs(x = NULL,
         y = "Observed counts", fill = NULL) +
# theme_minimal() +
    theme(plot.background = element_rect(fill = "transparent", colour = "transparent"),
          panel.background = element_rect(fill = "transparent", colour = "transparent"),
          panel.grid = element_blank()
    )
pp <- p1 +
annotation_custom(grob = ggplotGrob(p2),
                                xmin = -180,
                                xmax = -75,
                                ymin = -75,
                                ymax = 35)
pp


# save plot
ggsave("fig3.pdf", width = 10, height = 5.8, bg = "transparent")



# FIG4
grpcols <- c("#e5933b", "#4d8687", "gray", "#4c5c79", "#db4d4c", "#662c69", "#932834")

p1 <- alldf1 %>%
    filter(!is.na(pop_65)) %>%
    ggplot(aes(x = pop_65 * 100, y = IP_R)) +
    scale_y_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
   labels = scales::trans_format("log10", scales::math_format(10^.x))
                  ) +
    scale_x_continuous(breaks = c(0:6) * 5, labels = c(0:6) * 5) +
    geom_point(aes(fill = grp), size = 3) +
    geom_smooth(method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.5, 0.8, 0.2), linetype = 1) +
    geom_smooth(data = alldf1 %>% filter(!iscn), method = "lm", colour = rgb(0.8, 0.5, 0.3), fill = rgb(0.8, 0.5, 0.3, 0.2), linetype = 1) +
    labs(x = "Proportion of population aged over 65 (%)",
         # y = expression("Transmission rate (cases"~day^{-1}~M^{-1}~")"),
         y = expression(~R[I]~"(cases"~M^{-1}~day^{-1}~")"),
         # tilte = "b",
         fill = NULL) +
ggtitle("a") +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
theme(legend.position = "none",
      legend.background = element_rect(fill = NA)) +
    guides(fill = guide_legend(nrow=1, byrow=TRUE))
p2 <- alldf1 %>%
    filter(!is.na(life_exp)) %>%
    ggplot(aes(x = life_exp, y = IP_R)) +
    scale_y_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
   labels = scales::trans_format("log10", scales::math_format(10^.x))
                  ) +
    geom_point(aes(fill = grp), size = 3) +
    geom_smooth(method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.5, 0.8, 0.2), linetype = 1) +
    geom_smooth(data = alldf1 %>% filter(!iscn), method = "lm", colour = rgb(0.8, 0.5, 0.3), fill = rgb(0.8, 0.5, 0.3, 0.2), linetype = 1) +
    labs(x = "Life expectancy (years)",
         # y = expression("Transmission rate (cases"~day^{-1}~M^{-1}~")"),
         y = expression(~R[I]~"(cases"~M^{-1}~day^{-1}~")"),
         fill = NULL) +
ggtitle("b") +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
theme(legend.position = "right",
      legend.background = element_rect(fill = NA)) +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank())
p3 <- alldf1 %>%
    filter(!is.na(IP_rh)) %>%
    ggplot(aes(x = IP_rh, y = IP_R)) +
    scale_y_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
   labels = scales::trans_format("log10", scales::math_format(10^.x))
                  ) +
    scale_x_continuous(breaks = c(1:10) * 10) +
    geom_point(aes(fill = grp), size = 3) +
    # geom_smooth(method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.3, 0.3, 0.4)) +
    geom_smooth(method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.5, 0.8, 0.2), linetype = 1) +
    geom_smooth(data = alldf1 %>% filter(!iscn), method = "lm", colour = rgb(0.8, 0.5, 0.3), fill = rgb(0.8, 0.5, 0.3, 0.2), linetype = 1) +
    labs(x = "Relative humidity (%)",
         # y = expression("Transmission rate (cases"~day^{-1}~M^{-1}~")"),
         y = expression(~R[I]~"(cases"~M^{-1}~day^{-1}~")"),
         tilte = NULL,
         fill = NULL) +
ggtitle("c") +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
theme(legend.position = "right",
      legend.direction = "vertical",
      legend.background = element_rect(fill = NA)) +
    guides(fill = guide_legend(ncol=1, bycol=TRUE))
p4 <- alldf1 %>%
    filter(!is.na(IP_temp)) %>%
    ggplot(aes(x = IP_temp, y = IP_R)) +
    scale_y_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))
                  ) +
    scale_x_continuous(breaks = c(-3:7) * 5) +
    geom_point(aes(fill = grp), size = 3) +
    geom_smooth(method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.5, 0.8, 0.2), linetype = 1) +
    geom_smooth(data = alldf1 %>% filter(!iscn), method = "lm", colour = rgb(0.8, 0.5, 0.3), fill = rgb(0.8, 0.5, 0.3, 0.2), linetype = 1) +
    labs(x = "Air temperature (°C)",
         y = expression(~R[I]~"(cases"~M^{-1}~day^{-1}~")"),
         tilte = NULL,
         fill = NULL) +
ggtitle("d") +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
theme(legend.position = "none",
      legend.background = element_rect(fill = NA)) +
          guides(fill = guide_legend(nrow=1, byrow=TRUE)) +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank())
    p4
require(patchwork)
pp <- (p1 | p2 ) /
(p3 | p4) + 
# plot_annotation(tag_levels = "a") +
plot_layout(guides = 'collect')
pp

# save plot
ggsave("fig4.pdf", width = 7.2, height = 6, bg = "transparent")




figs7a <- alldf1 %>%
    ggplot(aes(x = pop_65 * 100, y = life_exp)) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
    # scale_x_log10(
                  # breaks = c(1, 2, 4, 8, 16, 32),
                  # # labels = str_c(c(1, 2, 4, 8, 16, 32), "%"),
                  # # breaks = scales::trans_breaks("log10", function(x) 10^x),
                  # # labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  # # limit = c(0.01, 1000)
                  # ) +
    geom_point(aes(fill = grp), size = 3.5) +
    geom_smooth(method = "lm") +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
    ggtitle("a") +
    theme(legend.position = "none",
          legend.background = element_rect(fill = NA)) +
    labs(x = "Proportion of population aged over 65 (%)", y = "Life expectancy (years)", fill = NULL)
figs7a
figs7b <- alldf1 %>%
    ggplot(aes(x = pop_65 * 100, y = hos_beds)) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
    # scale_x_log10(
                  # breaks = c(1, 2, 4, 8, 16, 32),
                  # # labels = str_c(c(1, 2, 4, 8, 16, 32), "%"),
                  # # breaks = scales::trans_breaks("log10", function(x) 10^x),
                  # # labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  # # limit = c(0.01, 1000)
                  # ) +
    geom_point(aes(fill = grp), size = 3.5) +
    geom_smooth(method = "lm") +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
    ggtitle("b") +
    theme(legend.position = "none",
          legend.background = element_rect(fill = NA)) +
    labs(x = "Proportion of population aged over 65 (%)", y = "Hospital beds per 1000 capita", fill = NULL)
figs7b
figs7c <- alldf1 %>%
    filter(!is.na(gdp)) %>%
    filter(gdp > 0) %>%
    mutate(gdppercap = gdp / pop) %>%
    ggplot(aes(x = pop_65 * 100, y = gdppercap)) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
    # scale_x_log10(
                  # breaks = c(1, 2, 4, 8, 16, 32),
                  # # labels = str_c(c(1, 2, 4, 8, 16, 32), "%"),
                  # # breaks = scales::trans_breaks("log10", function(x) 10^x),
                  # # labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  # # limit = c(0.01, 1000)
                  # ) +
    scale_y_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  # limit = c(0.01, 1000)
                  ) +
    geom_point(aes(fill = grp), size = 3.5) +
    geom_smooth(method = "lm") +
    ggtitle("b") +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
    labs(x = "Proportion of population aged over 65 (%)", y = "GDP per capita (USD)", fill = NULL)
figs7c


figs7 <- figs7a | figs7c +
plot_layout(guides = 'collect')
figs7


# save plot
ggsave("../figures/figs7.pdf", width = 8, height = 3.5, bg = "transparent")


alldf1 %>%
    filter(!is.na(gdp)) %>%
    filter(gdp > 0) %>%
    mutate(gdppercap = gdp / pop) %>%
    lm(log(gdppercap) ~ pop_65, data = .) %>%
    summary

alldf1 %>%
    lm(hos_beds ~ pop_65, data = .) %>%
    summary

alldf1 %>%
    lm(life_exp ~ pop_65, data = .) %>%
    summary


## FIGS8

p6 <- alldf1 %>%
    filter(!is.na(gdp)) %>%
    filter(gdp > 0) %>%
    ggplot(aes(x = gdp / pop, y = IP_R)) +
    scale_x_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))
                  ) +
    scale_y_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))
                  ) +
    geom_point(aes(fill = grp), size = 3) +
    geom_smooth(method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.5, 0.8, 0.2), linetype = 1) +
    geom_smooth(data = alldf1 %>% filter(!iscn), method = "lm", colour = rgb(0.8, 0.5, 0.3), fill = rgb(0.8, 0.5, 0.3, 0.2), linetype = 1) +
    labs(x = "GDP per capita (USD)",
         y = ~R[I]~"(cases"~M^{-1}~day^{-1}~")",
         tilte = NULL,
         fill = NULL) +
ggtitle("a") +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
theme(legend.position = "none",
      legend.background = element_rect(fill = NA)) +
    guides(fill = guide_legend(nrow=1, byrow=TRUE)) +
    theme(axis.title.y = element_blank())
p6
p8 <- alldf1 %>%
    filter(!is.na(area)) %>%
    ggplot(aes(x = area / pop, y = IP_R)) +
    scale_x_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))
                  ) +
    scale_y_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))
                  ) +
    geom_point(aes(fill = grp), size = 3) +
    geom_smooth(method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.5, 0.8, 0.2), linetype = 1) +
    geom_smooth(data = alldf1 %>% filter(!iscn), method = "lm", colour = rgb(0.8, 0.5, 0.3), fill = rgb(0.8, 0.5, 0.3, 0.2), linetype = 1) +
    labs(x = expression("Area per capita ("~km^2~")"),
         y = ~R[I]~"(cases"~M^{-1}~day^{-1}~")",
         tilte = NULL,
         fill = NULL) +
ggtitle("b") +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
theme(legend.position = "none",
      legend.background = element_rect(fill = NA)) +
    guides(fill = guide_legend(nrow=1, byrow=TRUE)) +
    theme(axis.title.y = element_blank())
icudf <- alldf1 %>%
    filter(!(country %in% c("Canada", "US"))) %>%
    group_by(country, grp) %>%
    dplyr::summarize(IP_R = mean(IP_R, na.rm = T),
                     icu_beds = mean(icu_beds, na.rm = T)
                     ) %>%
    bind_rows(alldf1 %>% filter(country %in% c("Canada", "US")) %>% select(country, grp, IP_R)) %>%
    filter(!is.nan(icu_beds))
p10 <- alldf1 %>%
    filter(!is.na(IP_wdsp)) %>%
    ggplot(aes(x = IP_wdsp, y = IP_R)) +
    scale_x_continuous(breaks = 0:10) +
    scale_y_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))
                  ) +
    geom_point(aes(fill = grp), size = 3) +
    geom_smooth(method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.5, 0.8, 0.2), linetype = 1) +
    geom_smooth(data = alldf1 %>% filter(!iscn), method = "lm", colour = rgb(0.8, 0.5, 0.3), fill = rgb(0.8, 0.5, 0.3, 0.2), linetype = 1) +
    labs(x = "Wind speed (m"~s^{-1}~")",
         y = ~R[I]~"(cases"~M^{-1}~day^{-1}~")",
         tilte = NULL,
         fill = NULL) +
ggtitle("c") +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
theme(legend.position = "none",
      legend.background = element_rect(fill = NA)) +
    guides(fill = guide_legend(nrow=1, byrow=TRUE)) +
    theme(axis.title.y = element_blank())
p10
p11 <- alldf1 %>%
    filter(!is.na(IP_visib)) %>%
    mutate(vibgrp = as.factor(ifelse(IP_visib < 20, " ", "  "))) %>%
    ggplot(aes(x = IP_visib, y = IP_R)) +
    scale_x_continuous(breaks = 0:9 * 5) +
    scale_y_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))
                  ) +
geom_vline(xintercept = 20, linetype = 2, colour = "gray") +
    geom_point(aes(fill = grp), size = 3) +
    geom_smooth(data = alldf1 %>% filter(IP_visib < 20),
                method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.5, 0.8, 0.2), linetype = 1) +
    geom_smooth(data = alldf1 %>% filter(!iscn) %>% filter(IP_visib < 20),
                method = "lm", colour = rgb(0.8, 0.5, 0.3), fill = rgb(0.8, 0.5, 0.3, 0.2), linetype = 1) +
    labs(x = "Visibility (km)",
         y = ~R[I]~"(cases"~M^{-1}~day^{-1}~")",
         tilte = NULL,
         fill = NULL) +
ggtitle("d") +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
theme(legend.position = "none",
      legend.background = element_rect(fill = NA)) +
    guides(fill = guide_legend(nrow=1, byrow=TRUE))
p11 <- alldf1 %>%
    filter(!is.na(IP_visib)) %>%
    # mutate(vibgrp = as.factor(ifelse(IP_visib < 20, " ", "  "))) %>%
    ggplot(aes(x = IP_visib, y = IP_R)) +
    scale_x_continuous(breaks = 0:9 * 5) +
    scale_y_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))
                  ) +
# geom_vline(xintercept = 20, linetype = 2, colour = "gray") +
    geom_point(aes(fill = grp), size = 3) +
    geom_smooth(
                method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.5, 0.8, 0.2), linetype = 1) +
    labs(x = "Visibility (km)",
         y = ~R[I]~"(cases"~M^{-1}~day^{-1}~")",
         tilte = NULL,
         fill = NULL) +
ggtitle("d") +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
theme(legend.position = "none",
      legend.background = element_rect(fill = NA)) +
    guides(fill = guide_legend(nrow=1, byrow=TRUE))
p11

pp <- (p6 | p8 ) /
(p10 | p11) + 
# plot_annotation(tag_levels = "a") +
plot_layout(guides = 'collect')
pp


# save plot

ggsave("../figures/figS8.pdf", width = 7.2, height = 6, bg = "transparent")


### stat

alldf1 %>%
    lm(log(IP_R) ~ pop_65, data = .)





# require(writexl)

# alldf1 %>%
    # select(PP_date, IP_date, GP_date) %>%
    # summarize_all(list(mean = ~round(mean(yday(.)), 0),
                       # q25 = ~round(quantile(yday(.), probs = c(0.25)), 0),
                       # q50 = ~round(quantile(yday(.), probs = c(0.50)), 0),
                       # q75 = ~round(quantile(yday(.), probs = c(0.25)), 0))) %>%
    # mutate_all(~(ymd(20191231) + .)) %>%
    # write_xlsx("stat_of_PPIPGP.xlsx")


hos_bedsgrplev <- quantile(alldf1$hos_beds, probs = c(0.5, 1), na.rm = T)
hos_bedsgrplab <- c("Low (≤ 2.9‰)", "High (> 2.9‰)")

hos_brk <- hos_bedsgrplev[1] %>% unname

hosbeddf <- alldf1 %>%
    filter(!is.na(hos_beds)) %>%
    mutate(hos_bedsgrp = unlist(lapply(hos_beds, function(x) {hos_bedsgrplev[min(which(x <= hos_bedsgrplev))]}))) %>%
    mutate(hos_bedsgrp = factor(hos_bedsgrp, levels = hos_bedsgrplev, labels = hos_bedsgrplab))


figs9 <- hosbeddf %>%
    mutate(fweekdf = purrr::map2(logisticp, FP_date, ~.x[[2]] %>% filter(date >= .y) %>% mutate(fweek = floor(as.numeric(date - .y) / 7) + 1) %>% group_by(fweek) %>% dplyr::summarize(R_IP = median(transpd, na.rm = T)))) %>%
    select(key, continent, country, grp, hos_bedsgrp, lat, long, fweekdf, FP_date) %>%
    unnest(fweekdf) %>%
    filter(fweek < 7) %>%
    ggplot(aes(x = fweek, y = R_IP)) +
    geom_jitter(colour='white', size = 2.3)+
    geom_violin(aes(group = fweek, fill = as.factor(fweek)), alpha = 0.5, trim=FALSE, width = 1) +
    stat_summary(fun.data = data_summary, shape = 21, size = .5, fill = "gray", colour = rgb(0.3, 0.3, 0.3)) +
    scale_y_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
   labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    # geom_point(aes(fill = grp), size = 3) +
    facet_wrap( ~ hos_bedsgrp, ncol = 2, scale = "fix") +
    geom_smooth(method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.3, 0.3, 0.4)) +
    labs(x = "Time relative to departure point (weeks)",
         y = expression(~R[t]~"(cases "~M^{-1}~day^{-1}~")"), fill = NULL) +
scale_x_continuous(breaks = 1:6) +
theme(legend.position = "none",
      legend.background = element_rect(fill = NA))
figs9

# save plot
ggsave("../figures/figS9.pdf", width = 6, height = 3, bg = "transparent")




## FIGS10

figs10a <- alldf1 %>%
    ggplot(aes(x = IP_temp, y = life_exp)) +
    geom_point(aes(fill = grp), size = 3) +
    geom_smooth(method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.5, 0.8, 0.2), linetype = 1) +
    geom_smooth(data = alldf1 %>% filter(!iscn), method = "lm", colour = rgb(0.8, 0.5, 0.3), fill = rgb(0.8, 0.5, 0.3, 0.2), linetype = 1) +
    scale_x_continuous(breaks = -4:8 * 5) +
    scale_y_continuous(breaks = 10:18 * 5) +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
    ggtitle("a") +
    labs(x = "Air temperature (°C)", y = "Life expectancy (years)", fill = NULL) +
    theme(legend.position = "none",
          legend.background = element_rect(fill = NA))
figs10b <- alldf1 %>%
    ggplot(aes(x = IP_temp, y = pop_65 * 100)) +
    geom_point(aes(fill = grp), size = 3) +
    geom_smooth(method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.5, 0.8, 0.2), linetype = 1) +
    geom_smooth(data = alldf1 %>% filter(!iscn), method = "lm", colour = rgb(0.8, 0.5, 0.3), fill = rgb(0.8, 0.5, 0.3, 0.2), linetype = 1) +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
    # scale_y_log10( breaks = c(1, 2, 4, 8, 16, 32)) +
    scale_x_continuous(breaks = -4:8 * 5) +
    ggtitle("b") +
    labs(x = "Air temperature (°C)", y = "Proportion of population aged over 65 (%)", fill = NULL) +
    theme(legend.position = "none",
          legend.background = element_rect(fill = NA))
figs10b
figs10c <- alldf1 %>%
    filter(!is.na(gdp)) %>%
    filter(gdp > 0) %>%
    ggplot(aes(x = IP_temp, y = gdp / pop)) +
    scale_y_log10( breaks = scales::trans_breaks("log10", function(x) 10^x),
   labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    geom_point(aes(fill = grp), size = 3) +
    geom_smooth(method = "lm", colour = rgb(0.3, 0.5, 0.8), fill = rgb(0.3, 0.5, 0.8, 0.2), linetype = 1) +
    geom_smooth(data = alldf1 %>% filter(!iscn), method = "lm", colour = rgb(0.8, 0.5, 0.3), fill = rgb(0.8, 0.5, 0.3, 0.2), linetype = 1) +
    scale_x_continuous(breaks = -4:8 * 5) +
scale_fill_manual(values = grpcols, breaks = c("AF", "AS", "CN", "EU", "NA", "OC", "SA")) +
    ggtitle("c") +
    labs(x = "Air temperature (°C)", y = "GDP per capita (USD)", fill = NULL)

figs10 <- figs10a | figs10b | figs10c + 
# plot_annotation(tag_levels = "a") +
plot_layout(guides = 'collect')
figs10

# save plot
ggsave("../figures/figS10.pdf", width = 10, height = 4, bg = "transparent")

## FIGS12 示意图

X <- seq(-3, 0, by = 0.01)
Y <- seq(1, 5.2, by = 0.01)
x <- seq(-3, 0, by = 0.1)
y <- seq(1, 5.2, by = 0.1)
trustgrid <- expand.grid(posrate = 10^x,
            testcaseperM = 10^y) %>%
mutate(trustp = log(testcaseperM / posrate))
trgrplev <- quantile(trustgrid$trustp, 1:10/10)
trustgrid <- trustgrid %>%
    mutate(trustpgrp = unlist(lapply(trustp, function(x) {trgrplev[min(which(x <= trgrplev))]}))) %>%
    mutate(trustpgrp = factor(trustpgrp, levels = trgrplev))
linedf1 <- tibble(posrate = 1:100 / 100,
                 trustp = 7) %>%
                 # trustp = trustpgrplev[1]) %>%
mutate(testcaseperM = exp(trustp) * posrate)
linedf2 <- tibble(posrate = 2:700 / 2000,
                 # trustp = trustpgrplev[2]) %>%
                 trustp = 12) %>%
mutate(testcaseperM = exp(trustp) * posrate)
labeldf <- tibble(posrate = c(34, 3.3, 0.3),
                  testcaseperM = c(50, 700, 6000),
                  label = c("Low", "Moderate", "High"))
figs12a <- trustgrid %>%
    ggplot(aes(x = posrate * 100, y = testcaseperM)) +
    geom_point(aes(fill = trustp), colour = rgb(0.5, 0.5, 0.5), size = 3) +
    geom_line(data = linedf1, aes(x = posrate * 100, y = testcaseperM),
              colour = rgb(0.3, 0.3, 0.3),
              # colour = rgb(0.3, 0.8, 0.5),
              linetype = 1, size = 1) +
    geom_line(data = linedf2, aes(x = posrate * 100, y = testcaseperM),
              colour = rgb(0.3, 0.3, 0.3),
              # colour = rgb(0.3, 0.8, 0.5),
              linetype = 1, size = 1) +
    # geom_point(aes(colour = trustpgrp), shape = 19) +
    geom_text(data = labeldf, aes(x = posrate, y = testcaseperM, label = label), angle = 40, size = 7, colour = "black", alpha = 0.9) +
    # geom_point(data = alldf1, aes(x = posrate * 100, y = testcaseperM, colour = grp), fill = "red") +
    scale_x_log10(limit = c(0.1, 100),
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  expand = c(0, 0)
                  ) +
    scale_y_log10(limit = c(10, 20000),
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  expand = c(0, 0)
                  ) +
    # scale_colour_discrete()
    scale_fill_gradient2(low = "blue", mid = "white", midpoint = 10, high = "red") +
    ggtitle("a") +
theme(legend.position = "none",
      legend.background = element_rect(fill = NA)) +
    guides(fill = guide_legend(nrow=1, byrow=TRUE)) +
    theme(axis.text.y = element_blank()) +
    theme(axis.text.x = element_blank()) +
    labs(x = "Positive rate of tested cases (%)", y = "Number of tested cases per million capita", fill = "Data reliability index") +
    theme(legend.position = "none",
          legend.background = element_rect(fill = NA))
figs12b <- alldf1 %>%
    filter(!is.na(trustpgrp)) %>%
    ggplot(aes(x = posrate * 100, y = testcaseperM)) +
    geom_point(aes(fill = trustpgrp), size = 3, colour = rgb(0.5, 0.5, 0.5)) +
    scale_x_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  ) +
    scale_y_log10( breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  ) +
scale_fill_manual(values = c(rgb(0.5, 0.5, 0.9), rgb(0.95, 0.95, 0.95), rgb(0.9, 0.5, 0.5))) +
    labs(x = "Positive rate of tested cases (%)", y = "Number of tested cases per million capita", fill = "Data reliability index") +
    ggtitle("b") +
    theme(legend.position = c(0.2, 0.8),
          legend.background = element_rect(fill = NA))
figs12 <- figs12a | figs12b +
plot_annotation(tag_levels = "a")
# plot_layout(guides = 'collect')
figs12

alldf1 %>%
    filter(trustpgrp == "High") %>%
    nrow

# save plot
ggsave("../figures/figs12.pdf", width = 9, height = 4.5, bg = "transparent")
