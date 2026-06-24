queryProm0 <- function(){
  paste0("


        WITH rand12_basis AS (

         -- Henter alt fra eprom datasettet
       SELECT
            MCEID, CENTREID, DATO_RAND12, RAND_1, RAND_2A, RAND_2B, RAND_3A, RAND_3B,
            RAND_4A, RAND_4B, RAND_5, RAND_6A, RAND_6B, RAND_6C, RAND_7,
            'elektronisk' AS besvart_rand12
            FROM rand12
            WHERE FOLLOWUP_PARENT_TYPE = 7 AND COMPLETE =1

            UNION ALL
            -- Henter alt fra det manuelle datasettet

         SELECT
            MCEID, CENTREID, DATO_RAND12, RAND_1, RAND_2A, RAND_2B, RAND_3A, RAND_3B,
            RAND_4A, RAND_4B, RAND_5, RAND_6A, RAND_6B, RAND_6C, RAND_7,
            'manuelt' AS besvart_rand12
            FROM rand12  AS b
            WHERE b.FOLLOWUP_PARENT_TYPE IN (1, 2, 3, 4)
            AND b.COMPLETE = 1
            AND NOT EXISTS (
              SELECT 1
              FROM rand12 AS a
              WHERE a.MCEID = b.MCEID
              AND a.FOLLOWUP_PARENT_TYPE = 7
            )

        )

         SELECT
            MCE.CENTREID,
            MCE.MCEID,
            MCE.PARENTMCEID,

            PROMS.TSSENDT AS proms_tssendt,
            PROMS.STATUS AS proms_status,
            PROMS.FORM_ORDER_STATUS_ERROR_CODE AS proms_form_order_status_error_code,
            PROMS.EXPIRY_DATE AS proms_expiry_date,


            # VARIALER FRA FOLLOWUP-SKJEMA
            BF.DATO_FOLLOWUP AS dato_followup,
            BF.COMPLETE AS followupbasis_complete,
            BF.INCOMPLETE_REASON AS followupbasis_incomplete_reason,
            BF.Q1 AS followupbasis_q1,
            BF.Q2 AS followupbasis_q2,
            BF.Q3 AS followupbasis_q3,
            BF.Q4 AS followupbasis_q4,
            BF.Q5 AS followupbasis_q5,
            BF.Q5_BURN_FREEZE AS followupbasis_q5_burn_freeze,
            BF.Q5_PACEMAKER AS followupbasis_q5_pacemaker,
            BF.Q5_ELECTROCONVERSION AS followupbasis_q5_electroconversion,
            BF.Q5_OTHER AS followupbasis_q5_other,
            BF.Q5_OTHER_SPECIFY AS followupbasis_q5_other_specify,
            BF.Q6 AS followupbasis_q6,
            BF.Q6_REGULAR_EKG AS followupbasis_q6_regular_ekg,
            BF.Q6_24_HOUR_EKG AS followupbasis_q6_24_hour_ekg,
            BF.Q6_PACEMAKER AS followupbasis_q6_pacemaker,
            BF.Q6_PULSE_WATCH AS followupbasis_q6_pulse_watch,
            BF.Q6_OTHER AS followupbasis_q6_other,
            BF.Q6_OTHER_SPECIFY AS followupbasis_q6_other_specify,
            BF.USERCOMMENT AS followupbasis_usercomment,
            BF.STATUS AS followupbasis_status,
            BF.TSCREATED AS followupbasis_tscreated,

            GKV.DATO_GKV AS dato_gkv,
            GKV.GKV_1 AS gkv_1,
            GKV.GKV_2 AS gkv_2,
            GKV.GKV_3 AS gkv_3,
            GKV.GKV_4 AS gkv_4,
            GKV.GKV_5 AS gkv_5,
            GKV.GKV_6 AS gkv_6,
            GKV.GKV_7 AS gkv_7,
            GKV.GKV_8 AS gkv_8,
            GKV.GKV_9 AS gkv_9,
            GKV.GKV_10 AS gkv_10,
            GKV.GKV_11 AS gkv_11,
            GKV.GKV_12 AS gkv_12,

            r.DATO_RAND12,
            r.besvart_rand12,
            r.RAND_1,
            r.RAND_2A,
            r.RAND_2B,
            r.RAND_3A,
            r.RAND_3B,
            r.RAND_4A,
            r.RAND_4B,
            r.RAND_5,
            r.RAND_6A,
            r.RAND_6B,
            r.RAND_6C,
            r.RAND_7

     FROM
      basisfollowup BF

      LEFT JOIN mce MCE ON MCE.MCEID = BF.MCEID AND MCE.MCETYPE = 7
      LEFT JOIN proms PROMS ON PROMS.MCEID = BF.MCEID
      LEFT JOIN gkv GKV ON GKV.MCEID = BF.MCEID
      LEFT JOIN rand12_basis AS r ON r.MCEID = BF.MCEID
  ")
}



noric::queryTaviprom
function(){

  paste0("
  SELECT
    MCE.CENTREID AS AvdRESH,
    MCE.MCEID AS ForlopsID,
    P.ID AS PasientID,
    proms.REGISTRATION_TYPE AS Registreringstype,
    P.SSN_TYPE AS FnrType,
    tavi.PROCEDUREDATE AS ProsedyreDato,

    proms.TSSENDT AS ePromBestillingsdato,
    proms.TSRECEIVED AS ePromMottatt,
    proms.EXPIRY_DATE AS ePromUtloeptDato,
    proms.STATUS AS ePromStatus,

    r.Q01 AS rose01,
    r.Q02 AS rose02,
    r.Q03 AS rose03,
    r.Q04 AS rose04,
    r.Q05 AS rose05,
    r.FORM_COMPLETED_VIA_PROMS AS roseFerdigViaProm,
    r.TSUPDATED AS roseDato,
    r.STATUS AS roseStatus,

    h.Q01 AS heart01 ,
    h.Q02 AS heart02 ,
    h.Q03 AS heart03 ,
    h.Q04 AS heart04 ,
    h.Q05 AS heart05 ,
    h.Q06 AS heart06 ,
    h.Q07 AS heart07 ,
    h.Q08 AS heart08 ,
    h.Q09 AS heart09 ,
    h.Q10 AS heart10,
    h.Q11 AS heart11,
    h.Q12 AS heart12,
    h.Q13 AS heart13,
    h.Q14 AS heart14,
    h.FORM_COMPLETED_VIA_PROMS AS heartFerdigViaProm,
    h.TSUPDATED AS heartDato,
    h.STATUS AS hearStatus,

    m.Q01 AS min01,
    m.Q02 AS min02,
    m.Q03 AS min03,
    m.Q04 AS min04,
    m.Q05 AS min05,
    m.Q06 AS min06,
    m.Q07 AS min07,
    m.Q08 AS min08,
    m.Q09 AS min09,
    m.Q10 AS min10,
    m.Q11 AS min11,
    m.Q12 AS min12,
    m.Q13 AS min13,
    m.Q14 AS min14,
    m.Q15 AS min15,
    m.Q16 AS min16,
    m.Q17 AS min17,
    m.Q18 AS min18,
    m.Q19 AS min19,
    m.Q20 AS min20,
    m.Q21 AS min21,
    m.FORM_COMPLETED_VIA_PROMS AS minFerdigViaProm,
    m.TSUPDATED AS minDato,
    m.STATUS AS minStatus,

    tav.Q01 AS tavi01,
    tav.Q01_2 AS tavi01_2,
    tav.Q02 AS tavi02,
    tav.Q02_2 AS tavi02_2,
    tav.Q03 AS tavi03,
    tav.Q04 AS tavi04,
    tav.Q05 AS tavi05,
    tav.Q06 AS tavi06,
    tav.Q06_2 AS tavi06_2,
    tav.Q07 AS tavi07,
    tav.FORM_COMPLETED_VIA_PROMS AS taviFerdigViaProm,
    tav.TSUPDATED AS taviDato,
    tav.STATUS AS taviStatus,

    prem.Q01 AS prem01,
    prem.Q02 AS prem02,
    prem.Q03 AS prem03,
    prem.Q04 AS prem04,
    prem.Q05 AS prem05,
    prem.Q06 AS prem06,
    prem.Q07 AS prem07,
    prem.Q08 AS prem08,
    prem.Q09 AS prem09,
    prem.Q10 AS prem10,
    prem.Q11 AS prem11,
    prem.Q12 AS prem12,
    prem.Q13 AS prem13,
    prem.Q14 AS prem14,
    prem.Q15 AS prem15,
    prem.FORM_COMPLETED_VIA_PROMS AS premFerdigViaProm,
    prem.TSUPDATED AS premDato,
    prem.STATUS AS premStatus

    FROM
      proms
      INNER JOIN mce MCE ON proms.MCEID = MCE.MCEID
      INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
      LEFT JOIN rose_dyspnea_scale r ON MCE.MCEID = r.MCEID
      LEFT JOIN heart_qol h ON MCE.MCEID = h.MCEID
      LEFT JOIN minnesota_questionnaire m ON MCE.MCEID = m.MCEID
      LEFT JOIN taviperc tavi ON MCE.MCEID = tavi.MCEID
      LEFT JOIN tavi_additional_questions tav ON MCE.MCEID = tav.MCEID
      LEFT JOIN prem ON MCE.MCEID = prem.MCEID
      WHERE proms.REGISTRATION_TYPE LIKE 'TAVI%'
")
