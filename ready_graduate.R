# Ready Graduate
# Evan Kramer
# 3/11/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd(str_c("N:/ORP_accountability/projects/", year(today()), "_ready_graduate/Data/"))

data = F
output = F
checks = F
domain = "ready_graduate"

# Load data
if(data) {
  rg = bind_rows(
    # AP
    read_csv("ap_student_level.csv"),
    # CIE
    read_csv("cie_student_level.csv"),
    # CLEP
    read_csv("clep_student_level.csv"),
    # DE
    read_csv("de_student_level.csv"),
    # IB
    read_csv("ib_student_level.csv"),
    # IC
    read_csv("ic_student_level.csv"),
    # LDC
    read_csv("ldc_student_level.csv"),
    # SDC
    read_csv("sdc_student_level.csv")
  ) %>% 
    arrange(student_key, epso_type) %>%
    group_by(student_key, epso_type) %>% 
    summarize(n_courses = max(n_courses, na.rm = T)) %>% 
    ungroup() %>%
    spread(epso_type, n_courses) %>% 
    # Merge with cohort
    right_join(read_csv(str_c("N:/ORP_accountability/data/", year(today()) - 1, "_graduation_rate/student_level.csv"),
                        col_types = "dccccTccTdcccccccdcdddcddcTcccdccccccdcTcccdTc") %>% 
                 arrange(desc(included_in_cohort), completion_type) %>% 
                 group_by(student_key) %>% 
                 summarize_at(vars(first_name:revised_included_in_cohort), funs(first(.))) %>% 
                 ungroup(), by = "student_key") %>% 
    # Merge ACT and SAT data
    left_join(read_dta(str_c("N:/ORP_accountability/data/", year(today()) - 1, "_ACT/Post-Appeals/", year(today()), 
                             "_ACT_student_level_actcohorthighest_appeals.dta")) %>% 
                select(student_key, ends_with("_highest"), sat_total), by = "student_key") %>% 
    # Compute Ready Graduate status
    mutate_at(vars(ap:sdc), funs(ifelse(is.na(.), 0, .))) %>%
    mutate(ready_graduate = case_when(
      included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13) & act_composite_highest >= 21 ~ "Y",
      included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13) & sat_total >= 1060 ~ "Y",
      included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13) & 
        ap + cie + clep + de + ib + ic + ldc + sdc >= 4 ~ "Y", 
      T ~ "N"
    )) %>% 
    transmute(
      student_key, first_name, middle_name, last_name, race_ethnicity, cohortyear, el, swd, ed, district_no = system,
      school_no = school, included_in_cohort, completion_type, sat_math = NA, sat_critical_reading = NA, sat_total, 
      act_english = act_english_highest, act_math = act_math_highest, act_reading = act_reading_highest, 
      act_science = act_science_highest, act_composite = act_composite_highest, industry_cert = ic, asvab = NA, 
      ncrc_work_keys = NA, n_cambridge = cie, n_ap = ap, n_ib = ib, n_sdc = sdc, n_ldc = ldc, n_de = de, clep, ready_graduate
    )  
} else {
  rm(data)
}

# Define path and filename
if(output) {
  path = str_c(getwd(), "/")
  file = str_c(domain, "_student_level.csv")
  
  if(file %in% list.files(path)) {
    if(!dir.exists(str_c(path, "Previous"))) {
      dir.create(str_c(path, "Previous"))
      dir.create(str_c(path, "Previous/", str_replace_all(now(), "[-:]", "")))
    }
    if(!dir.exists(str_c(path, "Previous/", str_replace_all(now(), "[-:]", "")))) {
      dir.create(str_c(path, "Previous/", str_replace_all(now(), "[-:]", "")))
    }
    file.rename(str_c(path, file),
                str_c(path, "Previous/", str_replace_all(now(), "[-:]", ""), "/", file))
  }
  write_csv(rg, str_c(path, file), na = "")  
} else {
  rm(output)
}

# Checks
if(checks) {
  # Confirm that RG business rules are applied correctly
  rg = as.tbl(
    dbGetQuery(
      eis_con,
      "select *
      from student_ready_grads"
    )
  ) %>% 
    janitor::clean_names()
  
  mutate_at(rg, vars(sat_math:participate_clg_lvl_pgm), funs(ifelse(is.na(.), 0, .))) %>% 
    mutate(should_be_rg = ifelse(sat_total >= 1060 | act_composite >= 21 | 
                                   industry_cert_earned + n_cambridge + n_adv_placement + 
                                   n_inter_baccalaureate + n_statewide_dual_credit + 
                                   n_local_dual_credit + n_dual_enrollment + participate_clg_lvl_pgm >= 4 | 
                                   (asvab >= 31 & industry_cert_earned + n_cambridge + n_adv_placement + 
                                      n_inter_baccalaureate + n_statewide_dual_credit + 
                                      n_local_dual_credit + n_dual_enrollment + participate_clg_lvl_pgm >= 2), "Y", "N"), 
         is_rg = ready_graduate) %>% 
    filter(should_be_rg == "Y" & is_rg == "N") %>%
    View()
    
    group_by(should_be_rg, is_rg) %>% 
    summarize(n = n())
  
  # Marked as ready in EIS but shouldn't be
  # Marked as ready in EIS and should be
  # Marked as not ready in EIS but should be
  # Marked as not ready in EIS and should not be
  
  
  con = dbConnect(JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
                  rstudioapi::askForPassword(prompt = "Please enter database credentials"),
                  rstudioapi::askForPassword(prompt = "Please enter schema name"),
                  rstudioapi::askForPassword(prompt = "Please enter password"))
                  
                  # "jdbc:oracle:thin:@//ag03sdcprdsh2.dcsouth.tenn:1540/CAEISPRD",
                  # "EIS_MGR", "N31aQw_8mXzPy_1")
  compare = full_join(
    rg,
    as.tbl(
      dbGetQuery(
        con,
        "select * 
      from student_ready_grads
      --from student_readygrads_score"
      )
    ) %>% 
      janitor::clean_names(),
    by = "student_key"
  ) %>% 
    # filter(industry_cert_earned > industry_cert) %>% # decision = overwrite
    # filter(act_reading.y > act_reading.x) %>%
    # filter(n_cambridge.y > n_cambridge.x) %>% 
    # filter(n_adv_placement > n_ap) %>%
    # filter(n_inter_baccalaureate > n_ib) %>% 
    # filter(n_statewide_dual_credit > n_sdc) %>%
    # filter(n_local_dual_credit > n_ldc) %>%
    # filter(n_dual_enrollment > n_de) %>% # decision = overwrite
    # filter(participate_clg_lvl_pgm > clep) %>% 
    # filter(ready_graduate.x == "Y" & ready_graduate.y == "N") %>%
    filter(ready_graduate.y == "Y" & ready_graduate.x == "N") %>% 
    select(student_key, starts_with("ready_graduate"))
  
  rg = read_csv(str_c(domain, "_student_level.csv"))
  # How many students were ready graduates? Because of ACT only? EPSO only? Both? 
  mutate(filter(rg, ready_graduate == "Y"),
         pathway_act_sat = act_composite >= 21 | sat_total >= 1060,
         pathway_epso = n_cambridge + n_ap + n_ib + n_sdc + n_ldc + n_de + clep >= 4,
         pathway_ic = industry_cert + n_cambridge + n_ap + n_ib + n_sdc + n_ldc + n_de + clep >= 4,
         pathway_asvab = NA) %>% 
    group_by(pathway_act_sat, pathway_epso, pathway_ic) %>% 
    summarize(n = n_distinct(student_key)) %>% 
    ungroup()
  
  # How do pre-appeals ready graduate percentages compare to last year (2017 grads)?
  # Are all ACT data reflected correctly? 
  # Will IT's ready_grad calculated field match our calculations?
  # Do all EPSOs seem to have normal spread?
  # for(e in c("industry_cert", "n_cambridge", "n_ap", "n_ib", "n_sdc", "n_ldc", "n_de", "clep")) {
  #   print(e); print(table(rg[, e]))
  # }
  
  # Perry County
  
  
  # Anderson County
  setwd(str_c("N:/ORP_accountability/projects/", year(today()), "_ready_graduate/Data"))
  (ac = read_csv("ready_graduate_student_level.csv") %>% 
    filter(district_no == 10) %>% 
    transmute(student_key, industry_cert, n_cambridge, n_ap, n_ib, n_sdc, n_ldc, n_de, clep,
              ready_graduate, date = file.mtime("ready_graduate_student_level.csv")))
  temp = tibble()
  for(f in list.files("Previous")) {
    for(f2 in list.files(str_c(getwd(), "/Previous/", f))) {
      if(str_detect(f2, "ready_graduate")) {
        temp = read_csv(str_c(getwd(), "/Previous/", f, "/", f2)) %>% 
          filter(district_no == 10) %>% 
          transmute(student_key, industry_cert, n_cambridge, n_ap, n_ib, n_sdc, n_ldc, n_de, clep,
                    ready_graduate, date = file.mtime(str_c(getwd(), "/Previous/", f, "/", f2)))
        ac = bind_rows(ac, temp) %>% 
          arrange(student_key, desc(date))
      }
    }
  }
  group_by(ac, student_key) %>% 
    summarize(n = n_distinct(ready_graduate)) %>%
    filter(n > 1) %>% 
    select(student_key) %>% 
    left_join(ac, by = "student_key") %>% 
    View()
} else {
  rm(checks)
}
