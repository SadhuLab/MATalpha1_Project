#National Institutes of Health: National Human Genome Research Institute, Division of Intramural Research, Section on Biology and Genome Engineering, Sadhu Lab
#Project: MATalpha1
#July 17, 2026
#R script: Performing statistical tests on chimera data
today_date <- Sys.Date()
formatted_date <- format(today_date, "%B %d, %Y")
cat("Emily Knisely-Durham", print(formatted_date), " \nNational Institutes of Health: National Human Genome Research Institute, Division of Intramural Research, Section on Biology and Genome Engineering, Sadhu Lab \nProject: Matalpha1 \nR script: Performing statistical tests on chimera data", "\n")

#Set working directory
setwd("C:/Users/kniselydurhamee/OneDrive - National Institutes of Health/Documents/Matalpha1 Code")

#Output path
output_path_stats <- "stats_output/MATalpha1_mosaic_stats.txt"

#source in data
source("~/MATalpha1 Code/micropub_figures_chimera_expression.R")

#Mann-Whitney U test for S. cerevisiae MATalpha 1 binding to S. cerevisiae promoter sequence
  #Group factors
  mannwhit_dataframe <- df3 %>%
    mutate(promoterchimera = paste(Promoter_sequence, MATalpha1_homolog))
  #Create vectors for data
scercontrol <- mannwhit_dataframe %>%
  filter(promoterchimera == "Scalpha1P Scalpha1")

candidatoscer <- mannwhit_dataframe %>%
  filter(promoterchimera == "Scalpha1P Caalpha1")

chimera1toscer <- mannwhit_dataframe %>%
  filter(promoterchimera == "Scalpha1P WC7")

chimera2toscer <- mannwhit_dataframe %>%
  filter(promoterchimera == "Scalpha1P WC1")

chimera3toscer <- mannwhit_dataframe %>%
  filter(promoterchimera == "Scalpha1P WC6")

chimera4toscer <- mannwhit_dataframe %>%
  filter(promoterchimera == "Scalpha1P WC5")

fig_c_test1 <- wilcox.test(scercontrol$GFP_expression, candidatoscer$GFP_expression, exact=TRUE)
fig_c_test2 <- wilcox.test(scercontrol$GFP_expression,chimera1toscer$GFP_expression, exact=TRUE)
fig_c_test3 <- wilcox.test(scercontrol$GFP_expression, chimera2toscer$GFP_expression, exact=TRUE)
fig_c_test4 <- wilcox.test(scercontrol$GFP_expression, chimera3toscer$GFP_expression, exact=TRUE)
fig_c_test5  <- wilcox.test(scercontrol$GFP_expression, chimera4toscer$GFP_expression, exact=TRUE)

#Mann-Whitney U test for C. albicans MATalpha 1 binding to C. albicans promoter sequence
  #Group factors
  mannwhit_df6 <- df6 %>%
    mutate(promoterchimera = paste(Promoter_sequence, MATalpha1_homolog))
  #Create vectors for data
  calbcontrol <- mannwhit_df6 %>%
    filter(promoterchimera == "Caalpha1P X")
  
  scertocalb <- mannwhit_df6 %>%
    filter(promoterchimera == "Caalpha1P Scalpha1")
  
  candidatocalb <- mannwhit_df6 %>%
    filter(promoterchimera == "Caalpha1P Caalpha1")
  
  chimera1tocalb <- mannwhit_df6 %>%
    filter(promoterchimera == "Caalpha1P WC7")
  
  chimera2tocalb <- mannwhit_df6 %>%
    filter(promoterchimera == "Caalpha1P WC1")
  
  chimera3tocalb <- mannwhit_df6 %>%
    filter(promoterchimera == "Caalpha1P WC6")
  
  chimera4tocalb <- mannwhit_df6 %>%
    filter(promoterchimera == "Caalpha1P WC5")
  
  fig_d_test1 <- wilcox.test(calbcontrol$GFP_expression, scertocalb$GFP_expression, exact=TRUE)
  fig_d_test2 <- wilcox.test(calbcontrol$GFP_expression, candidatocalb$GFP_expression, exact=TRUE)
  fig_d_test3 <- wilcox.test(calbcontrol$GFP_expression, chimera1tocalb$GFP_expression, exact=TRUE)
  fig_d_test4 <- wilcox.test(calbcontrol$GFP_expression, chimera2tocalb$GFP_expression, exact=TRUE)
  fig_d_test5  <- wilcox.test(calbcontrol$GFP_expression, chimera3tocalb$GFP_expression, exact=TRUE)
  fig_d_test6 <- wilcox.test(calbcontrol$GFP_expression, chimera4tocalb$GFP_expression, exact=TRUE)

#t-tests
  ttest1 <- t.test(scercontrol$GFP_expression, candidatoscer$GFP_expression, alternative = "greater")
  ttest2 <- t.test(scercontrol$GFP_expression, chimera1toscer$GFP_expression, alternative = "greater")
  ttest3 <- t.test(scercontrol$GFP_expression, chimera2toscer$GFP_expression, alternative = "greater")
  ttest4 <- t.test(scercontrol$GFP_expression, chimera3toscer$GFP_expression, alternative = "greater")
  ttest5 <- t.test(scercontrol$GFP_expression, chimera4toscer$GFP_expression, alternative = "greater")
  ttest6 <- t.test(calbcontrol$GFP_expression, scertocalb$GFP_expression, alternative = "less")
  ttest7 <- t.test(calbcontrol$GFP_expression, candidatocalb$GFP_expression, alternative = "less")
  ttest8 <- t.test(calbcontrol$GFP_expression, chimera1tocalb$GFP_expression, alternative = "less")
  ttest9 <- t.test(calbcontrol$GFP_expression, chimera2tocalb$GFP_expression, alternative = "less")
  ttest10 <- t.test(calbcontrol$GFP_expression, chimera3tocalb$GFP_expression, alternative = "less")
  ttest11 <- t.test(calbcontrol$GFP_expression, chimera4tocalb$GFP_expression, alternative = "less")
#save analyses in text file
sink(output_path_stats)

paste(fig_c_test1, fig_c_test2, fig_c_test3, fig_c_test4, fig_c_test5,
      fig_d_test1, fig_d_test2, fig_d_test3, fig_d_test5, fig_d_test6,
      ttest1, ttest2, ttest3, ttest4, ttest5,
      ttest6, ttest7, ttest8, ttest9, ttest10, ttest11)
sink()
