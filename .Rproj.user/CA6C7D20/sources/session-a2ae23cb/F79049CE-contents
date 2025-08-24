#======================================================
# 08/23 Appendixのデータの確認
#======================================================

# library
pacman::p_load(tidyverse,ggplot2,haven,quantreg,extremefit)
library(quantreg)
install.packages("extremefit")
library(extremefit)


# file path
dataframe_paths <- list.files( path ="Autor-Dorn-Hanson-ChinaSyndrome-FileArchive/dta", full.names=T)
dataframe_list <- lapply(dataframe_paths, read_dta)



#------------------------------------------------------
# #Table 1
#------------------------------------------------------
## Panel A

# read data set
Table1_data <- dataframe_list[[5]]
view(Table1_data)

Table1_data_90 <- Table1_data|>filter(yr==1990)
Table1_data_00 <- Table1_data|>filter(yr==2000)

quantile(Table1_data_90$d_tradeusch_pw, c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(Table1_data_00$d_tradeusch_pw, c(0.1, 0.25, 0.5, 0.75, 0.9))

probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# 90年のpercentile
weights_90 <- Table1_data_90$timepwt48
Table1_percentile_90 <- wquantile(Table1_data_90$d_tradeusch_pw, probs, weights_90)

# 2000年のpercentile
weights_00 <- Table1_data_00$timepwt48
Table1_percentile_00 <- wquantile(Table1_data_00$d_tradeusch_pw, probs, weights_00)
paste(Table1_percentile_00)


## Panel B
# 90年の上位40CZ
  Table1_data_90_B <- Table1_data_90 |>
    arrange(-l_popcount) |>
    slice(seq(1:40))|>
    arrange(-d_tradeusch_pw)

#
Panel_B_90  <- Table1_data_90_B |>select(city,d_tradeusch_pw)|>slice(c(seq(1,10),20,21,seq(31,40)))  


# 2000年の上位40CZ
city_key <- Table1_data_90_B|>
  select(city)

Table1_data_00_B <- Table1_data_00 |>
  select(city, d_tradeusch_pw) |>
  filter(city %in% city_key$city) |>
  arrange(-d_tradeusch_pw)

#
Panel_B_00  <- Table1_data_00_B |>slice(c(seq(1,10),20,21,seq(31,40))) 


#------------------------------------------------------
# #Table 2
#------------------------------------------------------
wa_1_4 <- sum(Table1_data_90$d_tradeusch_pw*Table1_data_90$timepwt48)
av_1_4 <- mean(Table1_data_90$d_tradeusch_pw)
sd_1_4 <- sqrt(sum((Table1_data_90$d_tradeusch_pw-av)^2*Table1_data_90$timepwt48))




l_tradeusch_pw00_2007 <- Table1_data_00$l_tradeusch_pw + Table1_data_00$d_tradeusch_pw*0.7

l_no_workers_totcbp_1990 <- Table1_data_90$l_no_workers_totcbp
l_no_workers_totcbp_2000 <- Table1_data_00$l_no_workers_totcbp

l_tradeusch_pw90_2007 <- l_tradeusch_pw00_2007*l_no_workers_totcbp_2000/l_no_workers_totcbp_1990

sum(l_tradeusch_pw90_2007*Table1_data_00$timepwt48)



l_trade_pw00_1991 <- l_trade_pw*l_no_workers_totcbp_1990/l_no_workers_totcbp_2000*(yr==1990)

#Table2_variables <- 
#sqrt(sum((Table1_data_90$d_tradeusch_pw - wa_1_4)^2)/length(Table1_data_90$d_tradeusch_pw))




