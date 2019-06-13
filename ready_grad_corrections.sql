/*
Ready Graduate Data Corrections
Evan Kramer
6/5/2019
*/

/*
-- Test changes
select * -- count(*)
from student_ready_grads
where ready_graduate = 'N' and industry_cert_earned + n_cambridge + n_adv_placement + n_inter_baccalaureate + n_statewide_dual_credit + 
    n_local_dual_credit + n_dual_enrollment + participate_clg_lvl_pgm >= 2 and asvab >= 31;
    
-- ACT/SAT scores: 3160 updates
update student_ready_grads
set ready_graduate = 'Y'
where ready_graduate = 'N' and (sat_total >= 1060 or act_composite >= 21);
commit;

-- 4 EPSOs: 2003 udpates
update student_ready_grads
set ready_graduate = 'Y'
where ready_graduate = 'N' and industry_cert_earned + n_cambridge + n_adv_placement + n_inter_baccalaureate + n_statewide_dual_credit + 
    n_local_dual_credit + n_dual_enrollment + participate_clg_lvl_pgm >= 4;
commit;

-- 2 EPSOs + ASVAB: 1 update
update student_ready_grads
set ready_graduate = 'Y'
where ready_graduate = 'N' and industry_cert_earned + n_cambridge + n_adv_placement + n_inter_baccalaureate + n_statewide_dual_credit + 
    n_local_dual_credit + n_dual_enrollment + participate_clg_lvl_pgm >= 2 and asvab >= 31;
commit;