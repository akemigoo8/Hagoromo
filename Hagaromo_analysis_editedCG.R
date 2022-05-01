---
  title: "hagaromo-data"
author: "Tanya Brown"
date: "3/11/2021"
output: html_document
# hehe
# on branch attempt
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r libraries}
library(tidyverse)
library(readxl)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(dplyr)
```
```
```
```{r things }
#set your working directory, where your file is located
#setwd('/Users/tanyabrown/Rasmussen Lab/Data analysis') #set up your working directory
setwd("C:/Users/goocam21/Desktop")
#read in your file and specific sheet
#read in your file: make sure that you have changed the sheet name and column name so it doesn't have spaces and column names match those in the function)


cghag_data <- read_excel("210311-hagaromo-MC.xlsx - Raw_data.xlsx",
                         'CGfull',
                       col_names = TRUE)
head(cghag_data)

#View(dist_data)

##hag_scale_data <-read_excel("210311-hagaromo-MC.xlsx - Raw_data.xlsx",
                            'Combfull', col_names = TRUE)
##head(hag_scale_data)
```

```{r cars}
cghag_sep <- cghag_data %>%
  separate('fish_name',
           into = c('exp_date', 'transgene', 'genotype', 'fish_id'), 
           sep = "-" )

head(cghag_sep)
```

```{r graphs}
cgg_order <- c("hag.sibs", "hag.mut")

cghag_sep %>% 
  ggplot(aes(x = factor(genotype, level = g_order), cell_density_mm2, fill = genotype)) +
  geom_boxplot()+
  geom_jitter(width = 0.15)+
  stat_compare_means()+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 1600),
                     breaks = seq(from = 0, to = 1600, by = 200))+
  theme_bw()+
  #theme(legend.position = 'none') +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  labs(title = 'zMC Cell Density (CGonly)', x = 'Genotype', y = 'zMC Density (cells per mm2)')+
  theme(plot.title = element_text(hjust=0.5))+
  scale_color_brewer('set 2')

#ggsave("/Users/tanyabrown/Rasmussen lab/Data analysis/filename.pdf", width = 6, height = 4, dpi = 300)
ggsave("/Users/goocam21/Desktop/mm2xgenoCGlarge.pdf", width = 6, height = 4, dpi = 300)

```
##Draw plot with TB and CG Large Image Data
hag_data<-read_excel("210311-hagaromo-MC.xlsx - Raw_data.xlsx",
                     'Combfull',
                     col_names = TRUE)
head(hag_data)

g_order <- c("hag.sibs", "hag.mut")

hag_sep %>% 
  ggplot(aes(x = factor(genotype, level = g_order), cell_density_mm2, fill = genotype)) +
  geom_boxplot()+
  geom_jitter(width = 0.15)+
  stat_compare_means()+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 1600),
                     breaks = seq(from = 0, to = 1600, by = 200))+
  theme_bw()+
  #theme(legend.position = 'none') +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  labs(title = 'zMC Cell Density', x = 'Genotype', y = 'zMC Density (cells per mm2)')+
  theme(plot.title = element_text(hjust=0.5))+
  scale_color_brewer('set 2')

```

```{r cars}
hag_sep <- cghag_data %>%
  separate('fish_name',
           into = c('exp_date', 'transgene', 'genotype', 'fish_id'), 
           sep = "-" )

head(hag_sep)
```

```{r graphs}
g_order <- c("hag.sibs", "hag.mut")

hag_sep %>% 
  ggplot(aes(x = factor(genotype, level = g_order), cell_density_mm2, fill = genotype)) +
  geom_boxplot()+
  geom_jitter(width = 0.15)+
  stat_compare_means()+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 1600),
                     breaks = seq(from = 0, to = 1600, by = 200))+
  theme_bw()+
  #theme(legend.position = 'none') +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  labs(title = 'zMC Cell Density', x = 'Genotype', y = 'zMC Density (cells per mm2)')+
  theme(plot.title = element_text(hjust=0.5))+
  scale_color_brewer('set 2')
Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

```{r}
#this function allows you to choose the genotypes you want to include in your analysis
data<-hag_sep

hag_subset_data <- function(data)
data_sep <- data %>%
  separate('fish_name',
           into = c('exp_date', 'transgene', 'genotype', 'fish_id'), 
           sep = "-" )

data_subset <- data_sep %>% select(fish_id, genotype, fish_mm, cell_density_mm2)
#new_data <- filter(data_subset, !grepl('hag.sibs', genotype))
hag_plot <- data %>%
  ggplot(aes(fish_mm, cell_density_mm2, 
             color = genotype, size = 1.5)) +
  scale_x_continuous(expand = c(0,0), limits = c(25,35) ) + 
  #breaks = seq(from = 6, to = 40, by = 4)) + #specify axes
  #scale_y_continuous(expand = c(0,0),
  #                 limits = c(0,1400),
  #                breaks = seq(from = 0, to = 1600, by = 200))+
  # scale_x_continuous(breaks = c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28)) + # specify the axes breaks
  #scale_y_continuous(breaks = c(0,400,800,1200)) +
  geom_jitter() +
  geom_smooth(method = lm, 
              se = TRUE, size =1) + #se = 95% confidence interval
  guides(size = "none") +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = 'black')) +
  #theme(legend.title = element_blank(), 
  #   legend.position= 'none') +
  labs(title = "Hag epAtoh1a+ Cell Density", 
       x = "Standard Length (mm)", 
       y = "epAtoh1a+ density (mm2)") +
  theme(plot.title = element_text(hjust =0.5))
#+ scale_color_brewer(palette = "Set11")
return(hag_plot)
hag_plot
ggsave("/Users/goocam21/Desktop/mm2xSLTBCG.pdf", width = 6, height = 4, dpi = 300)
```

```{r Johnson Neyman}
#load the packages and libraries:
#install.packages('interactions')
#install.packages('jtools')
#install.packages("sandwich")
library(jtools)
library(interactions)
library(sandwich)
#exclude WT data from analysis

hag_subset_data <- hag_data %>% separate('fish_name', 
                                         into = c('exp_date', 'transgene', 'genotype', 'fish_id'), 
                                         sep = "-")
h_mut_sib_data <-filter(hag_subset_data, !grepl('hag.sibs', genotype))

#change the data so that genotype is a numerical value bc JN technique struggles with factor moderators
hag_new_data <- hag_subset_data %>%
  mutate(gen = if_else(genotype == 'hag.mut', 1, 0))

#run your linear model and johnson-neyman stats
mod6 <- lm(cell_density_mm2~fish_mm*gen, data=hag_new_data)
h_ss <- sim_slopes(mod6, pred = gen, modx = fish_mm, johnson_neyman = TRUE)
hag_jn <- johnson_neyman(mod6, pred = gen, modx = fish_mm, plot = TRUE) #show the JN plot


#show your plot and simple slopes analysis
h_ss

```

```{r stuff}

cgscale_data<-read_excel("22.04.29_hagsmalldataCG.xlsx",
                         col_names = TRUE)

cgscale_data <- hag_scale_data %>%
  separate('fish_name',
           into = c('exp_date', 'transgene', 'genotype', 'fish_id'), 
           sep = "_" )

g_order <- c("hag.sibs", "hag.mut")


hag_scale_sep %>% 
  ggplot(aes(x = factor(genotype, lev = g_order), scale_density, fill = genotype)) +
  geom_boxplot()+
  geom_jitter(width = 0.2)+
  #stat_compare_means(label.y = 2000)+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 2400),
                     breaks = seq(from = 0, to = 2600, by = 400))+
  theme_bw()+
  theme(legend.position = 'none') +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  labs(title = 'zMC Cell Density per Scale', x = 'Genotype', y = 'zMC Density (cells per mm2)')+
  theme(plot.title = element_text(hjust=0.5))+
  scale_color_brewer('set 2')

#ggsave("/Users/tanyabrown/Rasmussen lab/Data analysis/filename.pdf", width = 6, height = 4, dpi = 300)
```
```{r}
# perform mann-whitney test between genotypes on large data
wilcox.test(cell_density_mm2~genotype, data=hag_sep)
```
```


```{r}
# perform mann-whitney test between genotypes on scale data
wilcox.test(scale_density~genotype, data=hag_scale_sep)
```


```{r}
```

