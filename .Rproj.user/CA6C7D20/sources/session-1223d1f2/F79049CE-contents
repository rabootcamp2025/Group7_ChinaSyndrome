#======================================================
# 08/23 Appendixのデータの確認
#======================================================

# library
pacman::p_load(tidyverse,ggplot2,haven,quantreg)


# file path
dataframe_paths <- list.files( path ="Autor-Dorn-Hanson-ChinaSyndrome-FileArchive/dta", full.names=T)
dataframe_list <- lapply(dataframe_paths, read_dta)



#------------------------------------------------------
# #Table 1
#------------------------------------------------------
# read data set
Table1_data <- dataframe_list[[5]]
view(Table1_data)

Table1_data_90 <- Table1_data|>filter(yr==1990)
Table1_data_00 <- Table1_data|>filter(yr==2000)

quantile(Table1_data_90$d_tradeusch_pw, c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(Table1_data_00$d_tradeusch_pw, c(0.1, 0.25, 0.5, 0.75, 0.9))

weights_90 <- Table1_data_90$timepwt48
wquantile(Table1_data_90$d_tradeusch_pw, weights_90, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))







