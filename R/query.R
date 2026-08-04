#' Ablanor SQL
#'
#' Get tables from database usig SQL. Notice, only tables when a
#' procedure-date exists!
#'

#' @return Data frame or (when multiple data sets are returned) a list of data
#' frames containing registry data. In case of \code{getNameReshId()} data may
#' also be returned as a named list of values (see Details).
#'
#' @name queryDataAblanor
#' @aliases queryProm0 queryRand12_0
NULL

#' @rdname queryDataAblanor
#' @export
queryProm0 <- function(){
  paste0("
         SELECT
            MCE.CENTREID,
            MCE.MCEID AS mceid_followupbasis,
            MCE.PARENTMCEID AS parentmceid,

            PROMS.TSSENDT AS proms_tssendt,
            PROMS.STATUS AS proms_status,
            PROMS.FORM_ORDER_STATUS_ERROR_CODE AS proms_form_order_status_error_code,
            PROMS.EXPIRY_DATE AS proms_expiry_date,

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
            GKV.GKV_12 AS gkv_12


     FROM
      basisfollowup BF

      LEFT JOIN mce MCE ON MCE.MCEID = BF.MCEID AND MCE.MCETYPE = 7
      LEFT JOIN proms PROMS ON PROMS.MCEID = BF.MCEID AND PROMS.REGISTRATION_TYPE = 'Basisfollowup'
      LEFT JOIN gkv GKV ON GKV.MCEID = BF.MCEID

    WHERE MCE.PARENTMCEID IS NOT NULL
  ")
}

#' @rdname queryDataAblanor
#' @export
queryRand12_0 <- function(){
    paste0("
      WITH rand12_basis AS (

    -- Henter RAND12 fra eprom datasettet
    -- (rand$mce for disse er mceid til OPPFOLGINGEN)
      (SELECT
        relectonic.CENTREID,
        relectonic.MCEID AS mceid_followupbasis,
        MCE.PARENTMCEID AS parentmceid,
        'elektronisk' AS besvart_rand12,
        DATO_RAND12, RAND_1, RAND_2A, RAND_2B, RAND_3A, RAND_3B,
        RAND_4A, RAND_4B, RAND_5, RAND_6A, RAND_6B, RAND_6C, RAND_7
      FROM
        rand12 relectonic
      LEFT JOIN mce MCE ON MCE.MCEID = relectonic.MCEID
      WHERE relectonic.FOLLOWUP_PARENT_TYPE = 7 AND relectonic.COMPLETE =1 AND MCE.PARENTMCEID IS NOT NULL)

      UNION ALL

      -- Henter RAND12 fra det manuelle datasettet (rand$mce for disse er mceid til PROSEDYREN)
      -- Med unntak av to prosedyrer (hardkodet) som også har ePROM

      (SELECT
        rmanual.CENTREID,
        NULL AS mceid_followupbasis,
        rmanual.MCEID AS parentmceid,
        'manuelt' AS besvart_rand12,
        DATO_RAND12, RAND_1, RAND_2A, RAND_2B, RAND_3A, RAND_3B,
        RAND_4A, RAND_4B, RAND_5, RAND_6A, RAND_6B, RAND_6C, RAND_7

      FROM rand12 rmanual
      WHERE rmanual.FOLLOWUP_PARENT_TYPE IN (1, 2, 3, 4)
      AND rmanual.COMPLETE = 1
      AND rmanual.MCEID NOT IN (26117, 26120)
      )
      )

      SELECT
       RAND.CENTREID,
       RAND.mceid_followupbasis,
       RAND.parentmceid,
       RAND.DATO_RAND12,
       RAND.besvart_rand12,
       RAND.RAND_1,
       RAND.RAND_2A,
       RAND.RAND_2B,
       RAND.RAND_3A,
       RAND.RAND_3B,
       RAND.RAND_4A,
       RAND.RAND_4B,
       RAND.RAND_5,
       RAND.RAND_6A,
       RAND.RAND_6B,
       RAND.RAND_6C,
       RAND.RAND_7

       FROM rand12_basis RAND
    ")
}


