#National Institutes of Health: National Human Genome Research Institute, Division of Intramural Research, Section on Biology and Genome Engineering, Sadhu Lab
#Project: Matalpha1
#June 10, 2026
#R script: Graphing GFP Expression of MATalpha1 Homolog Window Chimeras and their Promoter Sequences in MSY271
today_date <- Sys.Date()
formatted_date <- format(today_date, "%B %d, %Y")
cat("Emily Knisely-Durham", print(formatted_date), " \nNational Institutes of Health: National Human Genome Research Institute, Division of Intramural Research, Section on Biology and Genome Engineering, Sadhu Lab \nProject: Matalpha1 \nR script: Graphing GFP Expression of Matalpha1 Homolog Mixture Chimeras and their Promoter Sequences in MSY271", "\n")

#Loaded required packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

#Set working directory
setwd("C:/Users/kniselydurhamee/OneDrive - National Institutes of Health/Documents/Matalpha1 Code")

#Created output path
output_path_1 <- "GFP_output/GFP06012026_plot.png"
output_path_2 <- "GFP_output/GFPAVG06012026_plot.png"
output_path_3 <- "GFP_output/GFP06012026_plot+AVG.png"
output_path_4 <- "GFP_output/Sc_GFP06012026.png"
output_path_5 <- "GFP_output/Ca_GFP06012026.png"
output_path_6 <- "GFP_output/Sc_windows_GFP06012026.png"
output_path_7 <- "GFP_output/Ca_windows_GFP06012026.png"
output_path_8 <- "GFP_output/Sc_windows_GFP06092026.png"
output_path_9 <- "GFP_output/Ca_windows_GFP06092026.png"

#Read in csv file
file <- read.csv(file = "C:/Users/kniselydurhamee/OneDrive - National Institutes of Health/Documents/052926 Emily yeast plate GFP stats.csv", header = TRUE, sep = ',', quote = '')
df1 <- data.frame(file)
View(df1)
df1 <- df1[-91, ]

df2 <- data.frame(Promoter_sequence = c('Alone', 'Alone', 'Alone', 'Alone', 'Alone',
                                        'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P',
                                        'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P',
                                        'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P',
                                        'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P',
                                        'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 
                                        'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 
                                        'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 
                                        'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 'Scalpha1P', 
                                        'Scalpha1P', 'Scalpha1P', "Scalpha1P", "Scalpha1P", "Scalpha1P", 
                                        'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P',
                                        'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 
                                        'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 
                                        'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 
                                        'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 
                                        'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P',
                                        'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 
                                        'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P', 'Caalpha1P'), 
                  MATalpha1_homolog = c('X', 'X', 'X', 'X', 'X', 
                                        'X', 'X', 'X', 'X', 'X', 
                                        'Scalpha1', 'Scalpha1', 'Scalpha1', 'Scalpha1', 'Scalpha1', 
                                        'Caalpha1', 'Caalpha1', 'Caalpha1', 'Caalpha1', 'Caalpha1',
                                        'WC1', 'WC1', 'WC1', 'WC1', 'WC1',
                                        'WC3', 'WC3', 'WC3', 'WC3', 'WC3',
                                        'WC5', 'WC5', 'WC5', 'WC5', 'WC5', 
                                        'WC6', 'WC6', 'WC6', 'WC6', 'WC6',
                                        'WC7', 'WC7', 'WC7', 'WC7', 'WC7',
                                        'WC10', 'WC10', 'WC10','WC10', 'WC10', 
                                        'X', 'X', 'X', 'X', 'X', 
                                        'Scalpha1', 'Scalpha1', 'Scalpha1', 'Scalpha1', 'Scalpha1',
                                        'Caalpha1', 'Caalpha1', 'Caalpha1', 'Caalpha1', 'Caalpha1',
                                        'WC1', 'WC1', 'WC1', 'WC1', 'WC1', 
                                        'WC3', 'WC3', 'WC3', 'WC3', 'WC3',
                                        'WC5', 'WC5', 'WC5', 'WC5', 'WC5', 
                                        'WC6', 'WC6', 'WC6', 'WC6', 'WC6',
                                        'WC7', 'WC7', 'WC7', 'WC7', 'WC7'),
                  GFP_expression = c(df1$GFP....Parent))
View(df2)

#Created plot
p <- 
  df2 %>%
  arrange(GFP_expression) %>%
  mutate(MATalpha1_homolog = factor(MATalpha1_homolog, levels = c('Alone', 'X', 'Scalpha1', 'Caalpha1', 'WC1', 'WC3', 'WC5', 'WC6', 'WC7', 'WC10'))) %>%
  ggplot( aes(x= MATalpha1_homolog, y= GFP_expression, color= Promoter_sequence)) + geom_point(alpha = 0.5, size = 4) + theme_linedraw() + scale_color_manual(values =  c("Alone" = "black", "Scalpha1P" = "#619CFF", "Caalpha1P" = "purple"))
ggsave(filename = output_path_1, plot = p, width = 24, height =12, dpi = 300)
cat("Plot saved to", output_path_1, "\n")

#Separated data by promoter sequence
df3 <- data.frame(df2[1:50, ])
df4 <- data.frame(df2[1:5, ])
df5 <- data.frame(df2[51:90, ])
df6 <- rbind(df4, df5)

#Created plots
p <- 
  df3 %>%
  arrange(GFP_expression) %>%
  mutate(MATalpha1_homolog = factor(MATalpha1_homolog, levels = c('Alone', 'X', 'Scalpha1', 'Caalpha1', 'WC1', 'WC3', 'WC5', 'WC6', 'WC7', 'WC10'))) %>%
  ggplot( aes(x= MATalpha1_homolog, y= GFP_expression, color= Promoter_sequence)) + geom_point(alpha = 0.5, size = 4) + theme_linedraw() + scale_color_manual(values =  c("Alone" = "black", "Scalpha1P" = "#619CFF", "Caalpha1P" = "purple"))
ggsave(filename = output_path_4, plot = p, width = 24, height =12, dpi = 300)
cat("Plot saved to", output_path_4, "\n")

p <- 
  df6 %>%
  arrange(GFP_expression) %>%
  mutate(MATalpha1_homolog = factor(MATalpha1_homolog, levels = c('Alone', 'X', 'Scalpha1', 'Caalpha1', 'WC1', 'WC3', 'WC5', 'WC6', 'WC7'))) %>%
  ggplot( aes(x= MATalpha1_homolog, y= GFP_expression, color= Promoter_sequence)) + geom_point(alpha = 0.5, size = 4) + theme_linedraw() + scale_color_manual(values =  c("Alone" = "black", "Scalpha1P" = "#619CFF", "Caalpha1P" = "purple")) + labs()
ggsave(filename = output_path_5, plot = p, width = 24, height =12, dpi = 300)
cat("Plot saved to", output_path_5, "\n")

df3$GFP_expression <- parse_number(df3$GFP_expression)

df3_sum <- df3 %>% 
  group_by(Promoter_sequence, MATalpha1_homolog) %>%
  summarise(avgGFP = mean(GFP_expression))

df6$GFP_expression <- parse_number(df6$GFP_expression)

df6_sum <- df6 %>%
  group_by(Promoter_sequence, MATalpha1_homolog) %>%
  summarise(avgGFP = mean(GFP_expression))

w <- 
  df3 %>%
  arrange(GFP_expression) %>%
  mutate(MATalpha1_homolog = factor(MATalpha1_homolog, levels = c('Alone', 'X', 'Scalpha1', 'Caalpha1', 'WC1', 'WC3', 'WC4', 'WC5', 'WC6', 'WC7', 'WC10', 'WC11'))) %>%
  ggplot(aes(x= GFP_expression, y= MATalpha1_homolog, color = Promoter_sequence)) + geom_point(alpha = 0.5, size = 8) +
  labs(title = "Data Points and Averages", x = "GFP_expression", y = "MATalpha1_homolog") +
  theme_linedraw() + scale_color_manual(values =  c("Alone" = "black", "Scalpha1P" = "#619CFF", "Caalpha1P" = "#619")) +
  geom_point(data = df3_sum, aes(x = avgGFP, y = MATalpha1_homolog, color = Promoter_sequence), size = 9, shape = "|")
ggsave(filename = output_path_4, plot = w, width = 24, height =12, dpi = 300)
cat("Plot saved to", output_path_4, "\n")

w <- df6 %>%
  arrange(GFP_expression) %>%
  mutate(MATalpha1_homolog = factor(MATalpha1_homolog, levels = c('Alone', 'X', 'Scalpha1', 'Caalpha1', 'WC1', 'WC3', 'WC5', 'WC6', 'WC7'))) %>%
  ggplot(aes(x= GFP_expression, y= MATalpha1_homolog, color = Promoter_sequence)) + geom_point(alpha = 0.5, size = 8) +
  labs(title = "Data Points and Averages", x = "GFP_expression", y = "MATalpha1_homolog") +
  theme_linedraw() + scale_color_manual(values =  c("Alone" = "black", "Scalpha1P" = "#619CFF", "Caalpha1P" = "#619")) +
  geom_point(data = df6_sum, aes(x = avgGFP, y = MATalpha1_homolog, color = Promoter_sequence), size = 9, shape = "|")
ggsave(filename = output_path_5, plot = w, width = 24, height =12, dpi = 300)
cat("Plot saved to", output_path_5, "\n")

w <- 
  df3 %>%
  arrange(GFP_expression) %>%
  mutate(MATalpha1_homolog = factor(MATalpha1_homolog, levels = c('Alone', 'X', 'Scalpha1', 'Caalpha1', 'WC7', 'WC1', 'WC6', 'WC5', 'WC3', 'WC11', 'WC10', 'WC9'))) %>%
  ggplot(aes(x= GFP_expression, y= MATalpha1_homolog, color = Promoter_sequence)) + geom_point(alpha = 0.5, size = 8) +
  labs(title = "Data Points and Averages", x = "GFP_expression", y = "MATalpha1_homolog") +
  theme_linedraw() + scale_color_manual(values =  c("Alone" = "black", "Scalpha1P" = "#619CFF", "Caalpha1P" = "#619")) +
  geom_point(data = df3_sum, aes(x = avgGFP, y = MATalpha1_homolog, color = Promoter_sequence), size = 9, shape = "|")
ggsave(filename = output_path_6, plot = w, width = 24, height =12, dpi = 300)
cat("Plot saved to", output_path_6, "\n")

w <- df6 %>%
  arrange(GFP_expression) %>%
  mutate(MATalpha1_homolog = factor(MATalpha1_homolog, levels = c('Alone', 'X', 'Scalpha1', 'Caalpha1', 'WC3', 'WC11', 'WC10', 'WC9', 'WC7', 'WC1', 'WC6', 'WC5'))) %>%
  ggplot(aes(x= GFP_expression, y= MATalpha1_homolog, color = Promoter_sequence)) + geom_point(alpha = 0.5, size = 8) +
  labs(title = "Activation of C. albicans Promoter", x = "GFP_expression", y = "MATalpha1_homolog") +
  theme_linedraw() + scale_color_manual(values =  c("Alone" = "black", "Scalpha1P" = "#619CFF", "Caalpha1P" = "#619")) +
  geom_point(data = df6_sum, aes(x = avgGFP, y = MATalpha1_homolog, color = Promoter_sequence), size = 9, shape = "|")
ggsave(filename = output_path_7, plot = w, width = 24, height =12, dpi = 300)
cat("Plot saved to", output_path_7, "\n")

#June 10, 2026
today_date <- Sys.Date()
formatted_date <- format(today_date, "%B %d, %Y")

#data visualization for the yeast meeting
w <- 
  df3 %>%
  arrange(GFP_expression) %>%
  mutate(MATalpha1_homolog = factor(MATalpha1_homolog, levels = c('WC5', 'WC6', 'WC1', 'WC7', 'Caalpha1', 'Scalpha1', 'X', 'Alone'))) %>%
  ggplot(aes(x= GFP_expression, y= MATalpha1_homolog, color = Promoter_sequence)) + geom_point(alpha = 0.5, size = 8) +
  labs(title = "Activation of S. cerevisiae Promoter", x = "GFP_expression", y = "MATalpha1_homolog") +
  theme_linedraw() + scale_color_manual(values =  c("Alone" = "black", "Scalpha1P" = "#619CFF", "Caalpha1P" = "#619")) +
  geom_point(data = df3_sum, aes(x = avgGFP, y = MATalpha1_homolog, color = Promoter_sequence), size = 9, shape = "|")
ggsave(filename = output_path_8, plot = w, width = 24, height =12, dpi = 300)
cat("Plot saved to", output_path_8, "\n")

w <- df6 %>%
  arrange(GFP_expression) %>%
  mutate(MATalpha1_homolog = factor(MATalpha1_homolog, levels = c('WC5', 'WC6', 'WC1', 'WC7', 'Caalpha1', 'Scalpha1', 'X', 'Alone'))) %>%
  ggplot(aes(x= GFP_expression, y= MATalpha1_homolog, color = Promoter_sequence)) + geom_point(alpha = 0.5, size = 8) +
  labs(title = "Activation of C. albicans Promoter", x = "GFP_expression", y = "MATalpha1_homolog") +
  theme_linedraw() + scale_color_manual(values =  c("Alone" = "black", "Scalpha1P" = "#619CFF", "Caalpha1P" = "#619")) +
  geom_point(data = df6_sum, aes(x = avgGFP, y = MATalpha1_homolog, color = Promoter_sequence), size = 9, shape = "|")
ggsave(filename = output_path_9, plot = w, width = 24, height =12, dpi = 300)
cat("Plot saved to", output_path_9, "\n")

