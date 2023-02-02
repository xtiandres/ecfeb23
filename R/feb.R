# ELECCIONES 2023

# LIBRERIAS
library(dplyr)
library(tidyr)
library(plyr)
library(readr)
library(ggplot2)
library(vcd)
library(ggridges)
library(data.table)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
library(EpiEstim)

# DATASETS
dat <- read_csv('/home/xut/Documents/udaviz/R/studio/ecfeb23/data/data.csv')

# RESHAPE
dat1 <- gather(dat,
               key = 'variable',
               value = 'value',
               Comunicaliza:Cedatos)

# ALCALDIA UIO
auio <- filter(dat1,
               Dignidad == 'Alcaldia Quito')

# PLOT
#p1 <- ggplot(auio,
#             aes(x = value)) +
#  geom_histogram(fill = "cornflowerblue",
#                 color = "white") +
#  facet_grid(Candidato ~ variable)
#p1

# OTHER ONE
plotdata <- auio %>%
  group_by(Candidato, value, variable) %>%
  dplyr::summarize(n = n(),
            mean = mean(value),
            sd = sd(value),
            se = sd / sqrt(n))

# create better labels for discipline
#plotdata$discipline <- factor(plotdata$discipline,
#                              labels = c("Theoretical",
#                                         "Applied"))
# create plot
ggplot(plotdata, 
       aes(x = Candidato, 
           y = mean,
           color = Candidato)) +
#  geom_line(color = 'red') +
  geom_point(aes(color = Candidato, alpha = 1), size = 6) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .1) +
  scale_y_continuous(breaks = seq(2, 30, 2)) +
  aes(x = fct_inorder(Candidato)) +
  facet_wrap(~variable, ncol = 5) +
#  facet_grid(. ~variable + Candidato) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_color_brewer(palette="Set1")