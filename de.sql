select courses.isp_id, courses.student_key, courses.type_of_service, courses.first_name, courses.middle_name, courses.last_name,    
    courses.date_of_birth, courses.isp_school_year, courses.withdrawal_reason, courses.begin_date, courses.end_date,
    courses.sca_school_year, courses.sca_begin_date, courses.sca_end_date, courses.cs_school_year, 
    courses.sca_local_class_number, courses.course_code, courses.cs_begin_date, courses.cs_end_date, courses.state_dual_credit,
    courses.local_dual_credit, courses.dual_enrollment, courses.school_bu_id, courses.school_name,
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
        local_dual_credit, dual_enrollment, isp.school_bu_id, psl.school_name
    from instructional_service_period isp
    join student_class_assignment sca on sca.isp_id = isp.isp_id
    join class_section cs on 
        sca.instructional_program_num = cs.instructional_program_num and
        sca.local_class_number = cs.local_class_number and 
        sca.school_bu_id = cs.school_bu_id and 
        sca.school_year = cs.school_year   
    join post_secondary_sch_list psl on cs.post_secondary_institution = psl.school_code
    where cs.course_code in (
        4000,4001,4002,4003,4004,4005,4006,4007,4008,4009,4010,4011,4012,4013,4014,4015,4016,4017,4018,4019,4020,
        4021,4022,4023,4024,4025,4026,4027,4028,4029,4030,4031,4032,4033,4034,4035,4036,4037,4038,4039,4040,4041,
        4042,4043,4044,4045,4046,4047,4048,4049,4050,4051,4052,4053,4054,4055,4056,4057,4058,4059,4060,4061,4062,
        4063,4064,4065,4066,4067,4068,4069,4070,4071,4072,4073,4074,4075,4076,4077,4078,4079,4080,4081,4082,4083,
        4084,4085,4086,4087,4088,4089,4090,4091,4092,4093,4094,4095,4096,4097,4098,4099,4100,4101,4102,4103,4104,
        4105,4106,4107,4108,4109,4110,4111,4112,4113,4114,4115,4116,4117,4118,4119,4120,4121,4122,4123,4124,4125,
        4126,4127,4128,4129,4130,4131,4132,4133,4266,4267,4268,4272,4273,4274,4272,4273,4274,6189,6190,6191,6192
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