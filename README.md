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
- Establish memoranda of agreement with vendors, as needed
- Receive data files from vendors
	- Work with Jonathon Attridge to understand this process more completely
- Send data files to P20 to match to state student IDs
- Receive matched data from P20
- Compile initial Ready Graduate file for districts to review and appeal
- Run weekly updates and review district appeals
- Finalize data

## Code
The list below details the purpose of each script in this repository.
- [`ap.R`](https://github.com/evan-kramer/ready_graduate/blob/master/ap.R)
	- __Purpose__: Prepare AP course enrollment and exam data
	- __Frequency__: When source files are complete (typically November) and as needed afterwards to correct
- [`ap.sql`](https://github.com/evan-kramer/ready_graduate/blob/master/ap.sql)
	- __Purpose__: Pull AP course data from EIS (because the data pull takes a long time and can lock up R)
	- __Frequency__: When source files are complete (typically November) and as needed afterwards to correct
- [`app_checks.R`](https://github.com/evan-kramer/ready_graduate/blob/master/app_checks.R)
	- __Purpose__: Confirm that business rules are being applied correctly to the data in the Cohort application (e.g., that 4 early postsecondary opportunities make a student a Ready Graduate)
	- __Frequency__: Before and after each data release and as needed
- [`cambridge.R`](https://github.com/evan-kramer/ready_graduate/blob/master/cambridge.R)
	- __Purpose__: Prepare Cambridge course enrollment and exam data
	- __Frequency__: When source files are complete (typically November) and as needed afterwards to correct
- [`clep.R`](https://github.com/evan-kramer/ready_graduate/blob/master/clep.R)
	- __Purpose__: Prepare CLEP exam data
	- __Frequency__: When source files are complete (typically November) and as needed afterwards to correct
- [`de.sql`](https://github.com/evan-kramer/ready_graduate/blob/master/de.sql)
	- __Purpose__: Pull dual enrollment course data from EIS (because the data pull takes a long time and can lock up R)
	- __Frequency__: When source files are complete (typically November) and as needed afterwards to correct
- [`dual enrollment.R`](https://github.com/evan-kramer/ready_graduate/blob/master/dual_enrollment.R)
	- __Purpose__: Prepare dual enrollment course and exam data
	- __Frequency__: When source files are complete (typically November) and as needed afterwards to correct
- [`ib.R`](https://github.com/evan-kramer/ready_graduate/blob/master/ib.R)
	- __Purpose__: Prepare IB course enrollment and exam data
	- __Frequency__: When source files are complete (typically November) and as needed afterwards to correct
- [`industry_certifications.R`](https://github.com/evan-kramer/ready_graduate/blob/master/industry_certifications.R)
	- __Purpose__: Prepare industry certification data
	- __Frequency__: When source files are complete (typically November) and as needed afterwards to correct
- [`ldc.R`](https://github.com/evan-kramer/ready_graduate/blob/master/ldc.R)
	- __Purpose__: Prepare local dual credit course enrollment data
	- __Frequency__: When source files are complete (typically November) and as needed afterwards to correct
- [`ready_cohort.R`](https://github.com/evan-kramer/ready_graduate/blob/master/ready_cohort.R)
	- __Purpose__: Compile Ready Graduate information for the entire cohort for InformTN reporting
	- __Frequency__: When input files are complete (around February) and as needed afterwards to correct
	- __Note__: Have to re-run each time an individual input file needs to be re-run/corrected
- [`ready_grad_corrections.sql`](https://github.com/evan-kramer/ready_graduate/blob/master/ready_grad_corrections.sql)
	- __Purpose__: Make manual updates to Ready Graduate data in EIS when business rules are misapplied
	- __Frequency__: As needed
- [`ready_graduate.R`](https://github.com/evan-kramer/ready_graduate/blob/master/ready_graduate.R)
	- __Purpose__: Compile Ready Graduate information for reporting in the Cohort application
	- __Frequency__: When input files are complete (end of December) and as needed afterwards to correct
- [`rg_data_pull.R`](https://github.com/evan-kramer/ready_graduate/blob/master/rg_data_pull.R)
	- __Purpose__: Pull data from EIS to maintain an archive of Ready Graduate data.
	- __Frequency__: *Weekly* between the start of the appeals window (end of December) and the end of appeals (end of June)
	- __Note__: This script contains rules to deny appeals automatically and enter comments if either documentation or a score is missing; currently [this code](https://github.com/evan-kramer/ready_graduate/blob/master/rg_data_pull.R#L86) is commented out
- [`sdc.R`](https://github.com/evan-kramer/ready_graduate/blob/master/sdc.R)
	- __Purpose__: Prepare statewide dual credit course enrollment and exam data
	- __Frequency__: When source files are complete (typically November) and as needed afterwards to correct
- [`xw.R`](https://github.com/evan-kramer/ready_graduate/blob/master/xw.R)
	- __Purpose__: Create crosswalks to match EPSO types, course codes, and corresponding exam names
	- __Frequency__: When source files are complete (typically November) and as needed afterwards to correct