select courses.isp_id, courses.student_key, courses.type_of_service, courses.first_name, courses.middle_name, courses.last_name,    
    courses.date_of_birth, courses.isp_school_year, courses.withdrawal_reason, courses.begin_date, courses.end_date,
    courses.sca_school_year, courses.sca_begin_date, courses.sca_end_date, courses.cs_school_year, 
    courses.sca_local_class_number, courses.course_code, courses.cs_begin_date, courses.cs_end_date, courses.state_dual_credit,
    courses.local_dual_credit, courses.dual_enrollment, courses.school_bu_id, 
    instructional_days.district_no, instructional_days.school_no, instructional_days.id_date
from (
    select distinct student_key
    from studentcohortdata_historic
    where cohortyear = extract(year from sysdate) - 5 and completion_type in (1, 11, 12, 13)
) cohort
left outer join (
    select isp.isp_id, isp.student_key, isp.type_of_service, isp.first_name, isp.middle_name, isp.last_name,    
        isp.date_of_birth, isp.school_year as isp_school_year, isp.withdrawal_reason, isp.begin_date, isp.end_date,
        sca.school_year as sca_school_year, sca.sca_begin_date, sca.sca_end_date, cs.school_year as cs_school_year, 
        sca.local_class_number as sca_local_class_number, course_code, cs_begin_date, cs_end_date, state_dual_credit,
        local_dual_credit, dual_enrollment, isp.school_bu_id
    from instructional_service_period isp
    join student_class_assignment sca on sca.isp_id = isp.isp_id
    join class_section cs on 
        sca.instructional_program_num = cs.instructional_program_num and
        sca.local_class_number = cs.local_class_number and 
        sca.school_bu_id = cs.school_bu_id and 
        sca.school_year = cs.school_year
    where cs.course_code in (
        3013,3014,3025,3026,3036,3045,3055,3127,3128,3129,3145,3146,3147,3149,3150,3161,3162,3167,3168,3217,3225,3234,3236,3238,
        3239,3240,3440,3441,3443,3444,3445,3446,3447,3449,3450,3533,3534,3535,3544,3545,3634,3635
    ) 
) courses on cohort.student_key = courses.student_key
left outer join (
    select school_year, s.school_bu_id, s.district_no, s.school_no, sid.id_date
    from scal_id_days sid
    join school s on s.school_bu_id = sid.school_bu_id
    where school_year >= extract(year from sysdate) - 5
) instructional_days
on (
    courses.school_bu_id = instructional_days.school_bu_id and
    courses.isp_school_year = instructional_days.school_year
);