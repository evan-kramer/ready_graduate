# Ready Graduate Data Pull
# Evan Kramer
# 5/9/2019

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd(str_c("N:/ORP_accountability/projects/", year(today()), "_ready_graduate/"))

# Data
con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
  readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
  "EIS_MGR",
  readRegistry("Environment", "HCU")$EIS_MGR_PWD
) 
                
# Find most recent file
max_date = unique(list.files(str_c(getwd(), "/Data/Weekly Snapshots"))) %>% 
  str_replace_all("snapshot_", "") %>% 
  str_replace(".csv", "") %>% 
  ymd() %>% 
  max(na.rm = T)

# Query database for snapshots
rg = as.tbl(
  dbGetQuery(
    con, 
    "select * 
    from student_ready_grads"
  )
) %>% 
  janitor::clean_names() 
write_csv(rg, str_c(getwd(), "/Data/Weekly Snapshots/snapshot_", today(), ".csv"), na = "")

# Data updates and document uploads
updates = dplyr::setdiff(
  # Current data
  select(rg, student_key, sat_math:asvab, n_cambridge:participate_clg_lvl_pgm), #%>% 
  # Prior data 
  read_csv(str_c(getwd(), "/Data/Weekly Snapshots/snapshot_", max_date, ".csv")) %>%
    select(student_key, sat_math:asvab, n_cambridge:participate_clg_lvl_pgm) %>% 
    mutate_all(as.numeric)
) %>% 
  # Original data
  dplyr::setdiff(
    read_csv(str_c(getwd(), "/Data/ready_graduate_student_level.csv"), skip = 1, col_names = names(rg)[1:ncol(rg) - 1]) %>% 
      select(student_key, sat_math:asvab, n_cambridge:participate_clg_lvl_pgm) %>% 
      mutate_all(as.numeric)
  ) %>% 
  # Add document uploads
  full_join(
    dbGetQuery(
      con, 
      "select student_key, save_as_filename, modified_date, status, comments, reviewer_user_id, reviewed_date
      from student_readygrad_docs"
    ) %>% 
      janitor::clean_names(), by = "student_key"
  ) %>%
  # Add district and school numbers
  left_join(select(rg, student_key:school_no), by = "student_key") %>%
  # Compute summary statistics
  mutate(n_doc_no_data = !is.na(save_as_filename) & is.na(sat_math) & is.na(sat_critical_reading) & is.na(sat_total) & 
           is.na(act_english) & is.na(act_math) & is.na(act_reading) & is.na(act_science) & is.na(act_composite) & 
           is.na(industry_cert_earned) & is.na(asvab) & is.na(n_cambridge) & is.na(n_adv_placement) & 
           is.na(n_inter_baccalaureate) & is.na(n_statewide_dual_credit) & is.na(n_local_dual_credit) & 
           is.na(n_dual_enrollment) & is.na(participate_clg_lvl_pgm),
         n_docs_to_review = !is.na(save_as_filename) & is.na(status) & n_doc_no_data == F,
         n_data_no_doc = is.na(save_as_filename) & is.na(status),
         n_new_data = !is.na(sat_math) | !is.na(sat_critical_reading) | !is.na(sat_total) | 
           !is.na(act_english) | !is.na(act_math) | !is.na(act_reading) | !is.na(act_science) | !is.na(act_composite) | 
           !is.na(industry_cert_earned) | !is.na(asvab) | !is.na(n_cambridge) | !is.na(n_adv_placement) | 
           !is.na(n_inter_baccalaureate) | !is.na(n_statewide_dual_credit) | !is.na(n_local_dual_credit) | 
           !is.na(n_dual_enrollment) | !is.na(participate_clg_lvl_pgm))

# Output reviewer file
read_csv("N:/ORP_accountability/data/2018_final_accountability_files/system_names.csv") %>% 
  right_join(updates, by = c("system" = "district_no")) %>% 
  group_by(system, system_name) %>% 
  summarize_at(vars(n_doc_no_data:n_new_data), sum, na.rm = T) %>% 
  ungroup() %>%
  write_csv(str_c(getwd(), "/Documentation for Reviewers/Status Updates/status_update_", today(), ".csv"), na = "")

# Make database updates
# dbSendUpdate(
#   con,
#   "update student_readygrad_docs
#   set comments = NULL 
#   where status is null and comments is not null"
# )
# for(i in sort(unique(updates$student_key))) {
#   if(updates$n_data_no_doc[updates$student_key == i]) {
#     # No documentation
#     dbSendUpdate(
#       con,
#       str_c(
#         "update student_readygrad_docs ",
#         "set comments = 'No documentation has been uploaded for this appeal. Please upload documentation by clicking the Attach link or using the Mass Upload feature.' ",
#         "where status in (0, 2, NULL) and comments is null and student_key = ", i
#       ) 
#     )
#     # dbCommit(con)
#   } else if(updates$n_doc_no_data[updates$student_key == i]) {
#     # No data    
#     dbSendUpdate(
#       con,
#       str_c(
#         "update student_readygrad_docs ",
#         "set comments = 'No data has been uploaded for this appeal. Please upload updated data using the Mass Upload feature.' ",
#         "where status in (0, 2, NULL) and comments is null and student_key = ", i
#       ) 
#     )
#     # dbCommit(con)
#   } 
# }