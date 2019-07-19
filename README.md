# Ready Graduate Process

## Contents
- [Purpose](#purpose)
- [Sources](#sources)
- [Process](#process)
- [Code](#code)

## Purpose
This repository contains code for producing the student-level file for the [Tennessee Department of Education's](https://tn.gov/education) Ready Graduate indicator. You can learn more [here](https://www.tn.gov/education/career-and-technical-education.html).

The Ready Graduate indicator measures students' readiness for life after high school based on college entrance exam scores and completion of advanced coursework.

This measure aims to incentivize and recognize districts for preparing students for life after high school.

## Sources
The final file integrates the following data sources:
- [ACT or SAT](https://www.tn.gov/education/assessment/act-sat.html)
- [Advanced Placement](https://www.tn.gov/education/early-postsecondary/advanced-placement.html)
- [Cambridge International Examinations](https://www.tn.gov/education/early-postsecondary/cambridge-international.html)
- [College Level Examination Program](https://www.tn.gov/education/early-postsecondary/college-level-examination-program.html)
- [Dual Enrollment](https://www.tn.gov/education/early-postsecondary/dual-enrollment.html)
- [International Baccalaureate](https://www.tn.gov/education/early-postsecondary/international-baccalaureate.html)
- [Local Dual Credit](https://www.tn.gov/education/early-postsecondary/local-dual-credit.html)
- [Statewide Dual Credit](https://www.tn.gov/education/early-postsecondary/dual-credit.html)
- [ASVAB](http://www.official-asvab.com/)

## Process
The steps below outline the process of preparing and reporting Ready Graduate data at a high-level.
- Receive data files from vendors
- Send data files to P20 to match to state student IDs
- Receive matched data from P20
- Compile initial Ready Graduate file for districts to review and appeal
- Run weekly updates and review district appeals
- Finalize data

## Code
The list below details the purpose of each script in this repository.
- [`ap.R`]():
- [`ap.sql`]():
- [`app_checks`]():
- [`cambridge`]():
- [`clep`]():
- [`de.sql`]():
- [`dual enrollment`]():
- [`ib`]():
- [`industry_certifications`]():
- [`ldc`]():
- [`ready_cohort`]():
- [`ready_grad_corrections`]():
- [`ready_graduate`]():
- [`rg_data_pull`]():
- [`sdc`]():
- [`xw`]():
