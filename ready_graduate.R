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

data = T
output = F
checks = F
domain = "ready_graduate"

# Load data
if(data) {
  con = dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
    readRegistry("Environment", hive = "HCU")$EIS_MGR_CXN_STR[1],
    "EIS_MGR", 
    readRegistry("Environment", hive = "HCU")$EIS_MGR_PWD[1] 
  ) 
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
                arrange(student_key, desc(act_composite_highest), desc(act_math_highest), desc(act_reading_highest),
                        desc(act_english_highest), act_science_highest) %>%
                group_by(student_key) %>% 
                summarize_at(vars(ends_with("_highest"), sat_total), funs(first(.))), by = "student_key") %>% 
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
  
  rg_out = select(rg, student_key, n_sdc, ready_graduate) %>% 
    full_join(
      dbGetQuery(
        con,
        "select * 
        from student_ready_grads"
      ) %>% 
        janitor::clean_names(), 
      by = "student_key"
    ) %>% 
    mutate(n_statewide_dual_credit = pmax(n_sdc, n_statewide_dual_credit),
           ready_graduate = ifelse(ready_graduate.x == "Y" | ready_graduate.y == "Y", "Y", ready_graduate.x)) %>% 
    select(-n_sdc, -contains("."), -is_approved)
  names(rg_out) = names(rg)
  
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
  write_csv(rg_out, str_c(path, file), na = "")  
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
