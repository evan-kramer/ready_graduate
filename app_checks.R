# RG Application Checks
# Evan Kramer
# 6/5/2019

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(RJDBC)

rg = dbGetQuery(
  dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
    readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
    "EIS_MGR",
    readRegistry("Environment", "HCU")$EIS_MGR_PWD
  ),
  "select *
  from student_ready_grads"
) %>% 
  janitor::clean_names() %>% 
  as.tbl() 

# Check RG errors
for(v in names(rg)) {
  print(v)
  print(sum(is.na(rg[, v])))
}

# ACT
filter(rg, act_composite >= 21 & ready_graduate == "N")

# SAT
filter(rg, sat_total >= 1060 & ready_graduate == "N")

# 4 EPSOs
mutate_at(
  rg, 
  vars(industry_cert_earned:participate_clg_lvl_pgm),
  funs(ifelse(is.na(.), 0, .))
) %>% 
  filter(ready_graduate == "N" & industry_cert_earned + n_cambridge + 
           n_adv_placement + n_inter_baccalaureate + n_statewide_dual_credit + 
           n_local_dual_credit + n_dual_enrollment + participate_clg_lvl_pgm >= 4)

# 2 + ASVAB
mutate_at(
  rg, 
  vars(industry_cert_earned:participate_clg_lvl_pgm),
  funs(ifelse(is.na(.), 0, .))
) %>% 
  filter(ready_graduate == "N" & asvab >= 31 & industry_cert_earned + n_cambridge + 
           n_adv_placement + n_inter_baccalaureate + n_statewide_dual_credit + 
           n_local_dual_credit + n_dual_enrollment + participate_clg_lvl_pgm >= 2)