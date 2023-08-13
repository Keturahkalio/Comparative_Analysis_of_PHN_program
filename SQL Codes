SELECT n.NAM_SURNAME, n.nam_fore1, p.per_id, p.per_dob, b.ass_cli_id, b.ass_reason, p.per_gender, a.pifu_aboriginal, a.pifu_interpreter, e.PRAU_PHPRRACE, p.per_ethnicity, p.per_religion, b.ass_type, b.ass_date_of_ass, r.ref_type, r.ref_referred_on, d.pa0u_identify_fl, f.pi0u_cc_bool, f.pi0u_cbi_lu, p.per_imm_status, a.pifu_frsttmparent, 
a.pifu_someonetotlk, h.add_from, h.add_to, h.add_line4, h.add_type, h.add_postcode, i.ph1u_lsh_yn, a.pifu_completedhs, 
j.pepu_syspath, j.pepu_eligpath, f.PNFU_PATHWAY, a.pifu_incomediffi, a.pifu_incomeassist, a.pifu_depressed, a.pifu_lowinterest, a.pifu_tobacco, a.pifu_aroundsmoker

FROM database.table1 b
LEFT OUTER JOIN database.table2 a ON a.pifu_ass_id = b.ass_id
LEFT OUTER JOIN database.table3 f on b.ass_id=f.pnfu_ass_id
LEFT OUTER JOIN database.table4 c ON b.ass_cli_id = c.cli_id
LEFT OUTER JOIN database.table5 e on c.cli_per_id = e.PRAU_PER_ID
LEFT OUTER JOIN database.table6 p on p.per_id = c.cli_per_id 
LEFT OUTER JOIN database.table7 d on d.pa0u_per_id = c.cli_per_id
LEFT OUTER JOIN database.table8 f on f.pi0u_per_id = c.cli_per_id 
LEFT OUTER JOIN database.table9 g on g.alk_per_id = c.cli_per_id
LEFT OUTER JOIN database.table10 h on h.add_id = g.alk_add_id
LEFT OUTER JOIN database.table11 i on i.ph1u_per_id = c.cli_per_id 
LEFT OUTER JOIN database.table12 r on r.ref_cli_id = c.cli_id 
LEFT OUTER JOIN database.table13 j on j.pepu_ref_id = r.ref_id
LEFT OUTER JOIN database.table14 n on c.cli_per_id = n.NAM_PER_ID and n.NAM_TYPE = 'GIVEN'
  
Where(b.ass_date_of_ass > TO_DATE('2022-03-31','YYYY-MM,DD')) AND
b.ass_date_of_ass < TO_DATE('2023-04-01','YYYY-MM,DD') AND
b.ass_date_of_ass = REF_REFERRED_ON AND/
  ((b.ass_date_of_ass >= ADD_FROM AND b.ass_date_of_ass <= ADD_TO) or (b.ass_date_of_ass>=ADD_FROM and ADD_TO is null)) and 
  (b.ass_type = 'PHASPRENAT' OR b.ass_type = 'PHASCHCP01') AND r.ref_type ='PHRFPRENAT' AND 
  nvl( b.ass_reason, 'na' ) <> 'EIE'
