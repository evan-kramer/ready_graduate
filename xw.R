# Ready Graduate - Crosswalk
# Evan Kramer
# 1/7/2019

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd("N:/")

# Load data and connect to database
con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
  readRegistry("Environment", hive = "HCU")$EIS_MGR_CXN_STR[1], 
  "EIS_MGR", 
  readRegistry("Environment", hive = "HCU")$EIS_MGR_PWD[1] 
) 

# Course code-exam crosswalk
epso = readxl::read_excel("C:/Users/CA19130/Downloads/ed2356_course_code_2018-19.xlsx", sheet = 1) %>% 
  filter(str_detect(`Course Title`, "AP") | 
           str_detect(`Course Title`, "CIE") |
           str_detect(`Course Title`, "Dual Enrollment") |
           str_detect(`Course Title`, "IB") |
           str_detect(`Course Title`, "SDC")) %>% 
  transmute(
    epso_type = case_when(
      str_detect(`Course Title`, "AP") ~ "AP",
      str_detect(`Course Title`, "CIE") ~ "CIE",
      str_detect(`Course Title`, "Dual Enrollment") ~ "DE",
      str_detect(`Course Title`, "IB") ~ "IB",
      str_detect(`Course Title`, "SDC") ~ "SDC"
    ),
    course_code = as.numeric(`Course Code`), course_title = `Course Title`, exam_name = case_when(
      # AP
      course_code %in% c(3544) ~ "ART3D",
      course_code %in% c(3534) ~ "ARTHIS",
      course_code %in% c(3545) ~ "ARTS2",
      course_code %in% c(3533) ~ "ARTSTD",
      course_code %in% c(3217) ~ "BIOL",
      course_code %in% c(3127) ~ "CALCAB",
      course_code %in% c(3128) ~ "CALCBC",
      course_code %in% c(3225) ~ "CHEM",
      course_code %in% c(3145) ~ "CHINES",
      course_code %in% c(3635) ~ "COMSCA",
      course_code %in% c(3634) ~ "COMSCIP",
      course_code %in% c(3444) ~ "ECONMA",
      course_code %in% c(3443) ~ "ECONMI",
      course_code %in% c(3013) ~ "ENGLAN",
      course_code %in% c(3014) ~ "ENGLIT",
      course_code %in% c(3236) ~ "ENVSCI",
      course_code %in% c(3441) ~ "EURHIS",      
      course_code %in% c(3045) ~ "FRNLAN",
      course_code %in% c(3055) ~ "GERLA",
      course_code %in% c(3446) ~ "GOVCOM",
      course_code %in% c(3450) ~ "HUMGEO",
      course_code %in% c(3161) ~ "ITALIAN",
      course_code %in% c(3162) ~ "JAPAN",
      course_code %in% c(3036) ~ "LATINV",
      course_code %in% c(3535) ~ "MUSICT",
      course_code %in% c(3238) ~ "PHYS1",
      course_code %in% c(3239) ~ "PHYS2",
      course_code %in% c(3234) ~ "PHYSEM",
      course_code %in% c(3240) ~ "PHYSM",
      course_code %in% c(3447) ~ "PSYCH",
      course_code %in% c(3025) ~ "SPANLA",
      course_code %in% c(3026) ~ "SPANLIT",
      course_code %in% c(3129) ~ "STAT",
      course_code %in% c(3440) ~ "USHIST",
      course_code %in% c(3449) ~ "WDHIST",
      course_code %in% c(3445) ~ "GOVUS",
      
      # CIE
      course_code %in% c(4150, 4156) ~ "CAMBRIDGE BIOLOGY",
      course_code %in% c(4151, 4155) ~ "CAMBRIDGE CHEMISTRY",
      course_code %in% 4223:4225 ~ "CAMBRIDGE ENGLISH LANGUAGE",
      course_code %in% c(4154, 4255) ~ "CAMBRIDGE ENVIRONMENTAL MANAGEMENT",
      course_code %in% c(6260) ~ "CAMBRIDGE FIRST LANGUAGE SPANISH",
      course_code %in% c(4230:4231) ~ "CAMBRIDGE GENERAL PAPER",
      course_code %in% c(4218:4219) ~ "CAMBRIDGE GLOBAL PERSPECTIVES & RESEARCH",
      course_code %in% c(4195, 4196, 4198, 4205, 4206) ~ "CAMBRIDGE HISTORY",
      course_code %in% c(4228, 4229) ~ "CAMBRIDGE LITERATURE ENGLISH",
      course_code %in% c(4138:4144) ~ "CAMBRIDGE MATHEMATICS",
      course_code %in% c(4214:4216) ~ "CAMBRIDGE MUSIC",
      course_code %in% c(4199:4200) ~ "CAMBRIDGE PSYCHOLOGY",
      course_code %in% c(4220:4221) ~ "CAMBRIDGE THINKING SKILLS",
      course_code %in% c(4263:4264) ~ "CAMBRIDGE TRAVEL AND TOURISM",
      
      # IB
      course_code %in% c(3215, 3218, 3227, 3467, 6323:6328) ~ "BIO",
      course_code %in% c(3472, 3473, 6220, 6241, 6242, 6243) ~ "BUSMAN",
      course_code %in% c(3223, 3228, 3244, 3468, 6329, 6330, 6331, 6332, 6333, 6334) ~ "CHEM",
      course_code %in% c(3109, 3110, 6235, 6338, 6339, 6340) ~ "COMPSCI",
      course_code %in% c(6221, 6222, 6223, 6341, 6342, 6343) ~ "DANCE",
      course_code %in% c(3438, 6258, 6259, 6347, 6348, 6349) ~ "ECON",
      course_code %in% c(3004, 3485, 6351) ~ "ENGLISHA",
      course_code %in% c(6391:6393) ~ "ENGLISHLAL",
      course_code %in% c(3018, 6388, 6389, 6390) ~ "ENGLIT",
      course_code %in% c(3282, 3466) ~ "ENVIROSYS",
      course_code %in% c(3512, 3513, 3575, 6353, 6354, 6355) ~ "FILM",
      course_code %in% c(6394, 6395) ~ "FRENCHAB",
      course_code %in% c(3474, 3475) ~ "FRENCHB",
      course_code %in% c(6236, 6237) ~ "FURTHMATH",
      course_code %in% c(3283, 3439, 6249, 6359, 6360, 6361) ~ "GEOG",
      course_code %in% c(3163, 3164, 6362) ~ "GERMANA",
      course_code %in% c() ~ "GERMANAB",
      course_code %in% c(3163, 3164, 6362) ~ "GERMANALAL",
      course_code %in% c(3056, 3157, 3163, 3164, 3165, 6268, 6269, 6270, 6362, 6363, 6364, 6365) ~ "GERMANB",
      course_code %in% c(6248, 6250, 6251, 6366, 6367, 6368) ~ "GLOBPOL",
      course_code %in% c(3400, 3406, 3409, 3413, 3414, 3457, 3458, 3459, 3460, 3461, 3462, 3463, 3464, 3465, 3481, 6252, 6253, 6254, 6369, 6370, 6371, 6372, 6373, 6374, 6375) ~ "HIST",
      course_code %in% c(3104, 3105, 3106, 3138, 6404, 6405) ~ "IBMATH",
      course_code %in% c(3695, 3696, 6247, 6376, 6377, 6378) ~ "ITGS",
      course_code %in% c(3086, 3158, 6277, 6278, 6279, 6400, 6401, 6402) ~ "LATIN",
      course_code %in% c(3140, 3141) ~ "MATHSTUDY",
      course_code %in% c(3454, 3478, 3508, 3518, 6224, 6225) ~ "MUSIC",
      course_code %in% c(6255, 6256, 6257, 6406, 6407, 6408) ~ "PHILOSOPHY",
      course_code %in% c(3229, 3232, 3469, 6409, 6410, 6411, 6412, 6413, 6414) ~ "PHYSICS",
      course_code %in% c(3434, 3436, 3455, 6415, 6416, 6417) ~ "PSYCH",
      course_code %in% c(6424, 6425) ~ "SOCCULANTH",
      course_code %in% c(6396, 6397) ~ "SPANISHAB",
      course_code %in% c(3029, 3154, 3476, 3477, 3480, 6283, 6284, 6396, 6397, 6421, 6422, 6423) ~ "SPANISHB",
      course_code %in% c(3470, 3471) ~ "SPORTEXSCI",
      course_code %in% c(3482, 3487, 3488, 3546, 3547, 6426, 6427) ~ "THEATRE",
      course_code %in% c(3486, 3509, 3510, 3511, 3537, 3538, 3539, 3558, 3559, 3576, 6428) ~ "VISUALART",
      course_code %in% c(6429, 6430) ~ "WORLDRELIG",
      course_code %in% c(3437, 3557) ~ "WORLDSTUD",
      
      # SDC
      course_code %in% c(4270) ~ "IAGRIBUS",
      course_code %in% c(6434) ~ "AMHIST",
      course_code %in% c(6431) ~ "CRIM",
      course_code %in% c(4270) ~ "IAGRIBUS",
      course_code %in% c(4269) ~ "IPLANTSCI",
      course_code %in% c(6432) ~ "PRECALC",
      course_code %in% c(6436) ~ "PSYCH",
      course_code %in% c(4271) ~ "SOCIOLOGY",
      course_code %in% c(6433) ~ "STATISTICS",
      course_code %in% c(6435) ~ "WHISTGEOG"
    )
  ) %>% 
  arrange(epso_type, course_code)

# Output
path = str_c(getwd(), "ORP_accountability/projects/", 
             ifelse(between(month(today()), 1, 9), year(today()), year(today()) + 1),
             "_ready_graduate/Code/Crosswalks/")
file = "epso_course_codes_exams.csv"

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
write_csv(epso, str_c(path, file), na = "")

# Industry certification crosswalk
ic = readxl::read_excel("C:/Users/CA19130/Downloads/cte_certs_ESSA_conversion (4).xlsx") %>% 
  transmute(cert_name = `Industry Certification`, n_epso = case_when(
    str_detect(`TDOE ESSA Conversion`, "ONLY") ~ 2,
    str_detect(`TDOE ESSA Conversion`, "1 EPSO") ~ 3,
    str_detect(`TDOE ESSA Conversion`, "2 EPSO") ~ 4,
    str_detect(`TDOE ESSA Conversion`, "3 EPSO") ~ 4,
    str_detect(`TDOE ESSA Conversion`, "4 EPSO") ~ 4
  )) %>%
  arrange(cert_name, desc(n_epso)) %>% 
  group_by(cert_name) %>% 
  summarize(n_epso = max(n_epso, na.rm = T)) %>% 
  ungroup() 
write_csv(ic, "ORP_accountability/projects/2019_ready_graduate/Code/Crosswalks/industry_certification_conversion.csv", na = "")  

# Output
path = str_c(getwd(), "ORP_accountability/projects/", 
             ifelse(between(month(today()), 1, 9), year(today()), year(today()) + 1),
             "_ready_graduate/Code/Crosswalks/")
file = "industry_certification_conversion.csv"

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
write_csv(ic, str_c(path, file), na = "")

# domain = "sdc"; c = "worl"; filter(cc, str_detect(`Course Title`, str_to_title(c)) & str_detect(`Course Title`, str_to_upper(domain))) 
# str_flatten(filter(cc, str_detect(`Course Title`, str_to_title(c)) & str_detect(`Course Title`, str_to_upper(domain)))$`Course Code`, ", ")
# exams = read_delim("N:/ORP_accountability/data/2018_Assessment_Files/2014 cohort AP SDC IB CLEP raw.txt",
#                    delim = "\t", col_types = "ddcccccccccccccccc") %>%
#   janitor::clean_names() %>% 
#   filter(test_desc == "STATEWIDE DUAL CREDITS") %>%
#   mutate(test_administration_cd = str_replace_all(test_administration_cd, "[_0123456789]", "") %>% 
#            str_replace("SDC", "") %>% 
#            str_replace("FALL", "") %>% 
#            str_replace("SPRING", "") %>% 
#            str_replace("SPRIN", "") %>% 
#            str_replace("SPR", "")) %>% 
#   mutate(test_administration_cd = case_when(
#     test_administration_cd == "AGBUSFIN" ~ "IAGRIBUS",
#     test_administration_cd == "ISOCIOLOGY" ~ "SOCIOLOGY",
#     test_administration_cd == "GREENHOUSE" ~ "IPLANTSCI",
#     !is.na(test_administration_cd) ~ test_administration_cd
#   )) %>%
#   group_by(test_administration_cd) %>% 
#   summarize(n())