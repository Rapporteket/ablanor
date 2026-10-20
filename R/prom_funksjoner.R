#' Hjelpefunksjoner for PROM
#'

#' @param df Inneholder data fra pci + pciprom. Må inneholde
#' variabelen \code{kriterie_alle_midlertidig}.
#' @param registryName for SQL queries
#'
#' @name prom_funksjoner
#' @aliases kriterie_basisprom_siste_aar  kriterie_1yr_prom_siste_aar kriterie_ingen_ny_pros_neste_aar kriterie_5yr_prom_siste_5aar
#'
#'
#' @rdname prom_funksjoner
#' @export
kriterie_basisprom_siste_aar <- function(df,
                                         reshId = reshId,
                                         userRole = userRole){

  stopifnot(c("kriterie_basis_midlertidig") %in% names(df))

  # alle PID for pasienter  som har de øvrige kriteriene for prom
  # og som har prosedyredato etter prodsetting av basis-prom
  df_sub <- df  %>%
    dplyr::filter(
      kriterie_basis_midlertidig %in% "ja",
      dato_pros >= as.Date("2023-11-08", format = "%Y-%m-%d")) %>%
    dplyr::select(patient_id, dato_pros, mceid, mceid_followupbasis,
                  forlopstype, kriterie_basis_midlertidig)

  if(nrow(df_sub)>0){

    list_patients <- df_sub  %>%
      dplyr::distinct(patient_id)  %>%
      dplyr::pull(patient_id)  %>%
      stringr::str_flatten_comma(last = ", ")

    # Finne alle BASIS-prom for de utvalgte pasientene,
    # Legg til FORLOPSTYPE for prosedyren
    query = paste0("
       WITH
       basisprom AS (
           SELECT
              basisfollowup.`MCEID` as mceid_followupbasis_oppf,
              mce.`PATIENT_ID` AS patient_id,
              proms.`TSSENDT` as eprom_bestilt_dato,
              mce.`PARENTMCEID` as parentmceid_oppf
           FROM `basisfollowup`
           LEFT JOIN mce ON mce.MCEID = basisfollowup.MCEID AND MCETYPE = 7
           LEFT JOIN proms ON proms.`MCEID` = basisfollowup.MCEID and proms.REGISTRATION_TYPE = 'Basisfollowup'
           ),
       main AS (
           SELECT
               mce.MCEID as parentmceid_oppf,
               mce.MCETYPE as parent_mcetype
           FROM mce
           WHERE mce.MCETYPE IN (1, 2, 3, 4)
       )

       SELECT
          basisprom.mceid_followupbasis_oppf,
          basisprom.patient_id,
          basisprom.eprom_bestilt_dato,
          basisprom.parentmceid_oppf,
          main.parent_mcetype
       FROM
         basisprom
       LEFT JOIN
         main ON main.parentmceid_oppf = basisprom.parentmceid_oppf
       WHERE
         basisprom.patient_id IN ",
                   "(", list_patients, ")",
                   " ;")
    proms_basis <- rapbase::loadRegData(
      registryName = "data",
      query = query)

    # finne ALLE oppfølginer av samme forløptype som prosedyren,
    #  som har bestillingsdato for eprom FØR prosedyre og som ikke hører
    #  direkte til denne prosedyren
    # Dersom eprom var bestilt (av samme prosedyretype) det siste året, så
    # blir kriterie_ingen_basisprom = "nei".
    # NB - husk å gruppere på mceid (én prosedyre kan ha flere tidligere prom)
    fullmatch <- dplyr::inner_join(
      x = df_sub %>%
        dplyr::filter(dato_pros >= as.Date("2023-11-08", format = "%Y-%m-%d")) |>
        dplyr::select(-kriterie_basis_midlertidig),

      y = proms_basis,

      by = c("patient_id", "forlopstype" = "parent_mcetype"),

      relationship = "many-to-many") |>

      dplyr::filter(mceid_followupbasis != mceid_followupbasis_oppf |
                      !mceid_followupbasis %in% mceid_followupbasis_oppf,
                    dato_pros >= eprom_bestilt_dato) |>
      dplyr::filter(dato_pros >= as.Date("2022-11-08", format = "%Y-%m-%d"))  %>% # 1-år før innføring av basis prom
      dplyr::mutate(
        teoretisk_prom_lower = dato_pros %m-% months(12),
        teoretisk_prom_upper = dato_pros + 1,
        kriterie_ingen_basisprom_siste12mnd = ifelse(
          eprom_bestilt_dato > teoretisk_prom_lower &
            eprom_bestilt_dato <= teoretisk_prom_upper,
          "nei", NA_character_))



    return(
      dplyr::left_join(
        df,
        fullmatch %>%
          dplyr::group_by(mceid)  %>%
          dplyr::mutate(

            kriterie_ingen_basisprom_siste12mnd = ifelse(
              any(kriterie_ingen_basisprom_siste12mnd == "nei"),
              "nei",
              NA_character_),

            n = 1:dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::filter(n == 1) %>%
          dplyr::select(patient_id , mceid, kriterie_ingen_basisprom_siste12mnd),
        by = c("patient_id", "mceid"))
    )
  }
  if(nrow(df_sub) == 0) {
    return(df  %>%  dplyr::mutate(kriterie_ingen_basisprom_siste12mnd = NA_character_))
  }
}

#' @rdname prom_funksjoner
#' @export
kriterie_1yr_prom_siste_aar <- function(df,
                                        reshId = reshId,
                                        userRole = userRole){

  stopifnot(c("kriterie_1yr_midlertidig",
              "dato_followup_teoretisk_1aar") %in% names(df))

  # alle PID for pasienter som har de øvrige kriteriene for 1-års-prom
  # og som har prosedyredato etter prodsetting av  1-års-prom
  df_sub <- df  %>%
    dplyr::filter(kriterie_1yr_midlertidig %in% "ja") %>%
    dplyr::select(patient_id, dato_pros, mceid, mceid_followup1,
                  forlopstype, dato_followup_teoretisk_1aar)

  if(nrow(df_sub)>0){

    list_patients <- df_sub  %>%
      dplyr::distinct(patient_id)  %>%
      dplyr::pull(patient_id)  %>%
      stringr::str_flatten_comma(last = ", ")

    # Finne alle 1års-prom for de utvalgte pasientene,
    # Legg til FORLOPSTYPE for prosedyren
    query = paste0("
       WITH
       prom1yr AS (
           SELECT
              followup.`MCEID` as mceid_followup1yr_oppf,
              mce.`PATIENT_ID` AS patient_id,
              proms.`TSSENDT` as eprom_bestilt_dato,
              mce.`PARENTMCEID` as parentmceid_oppf
           FROM `followup`
           LEFT JOIN mce ON mce.MCEID = followup.MCEID AND MCETYPE = 9
           LEFT JOIN proms ON proms.`MCEID` = followup.MCEID and proms.REGISTRATION_TYPE = 'Followup'
           ),
       main AS (
           SELECT
               mce.MCEID as parentmceid_oppf,
               mce.MCETYPE as parent_mcetype
           FROM mce
           WHERE mce.MCETYPE IN (1, 2, 3, 4)
       )

       SELECT
          prom1yr.mceid_followup1yr_oppf,
          prom1yr.patient_id,
          prom1yr.eprom_bestilt_dato,
          prom1yr.parentmceid_oppf,
          main.parent_mcetype
       FROM
         prom1yr
       LEFT JOIN
         main ON main.parentmceid_oppf = prom1yr.parentmceid_oppf
       WHERE
         prom1yr.patient_id IN ",
                   "(", list_patients, ")",
                   " ;")
    proms_1yr <- rapbase::loadRegData(
      registryName = "data",
      query = query)

    # finne ALLE oppfølginer av samme forløptype som prosedyren,
    #  som har bestillingsdato for eprom FØR prosedyre og som ikke hører
    #  direkte til denne prosedyren
    # Dersom eprom var bestilt (av samme prosedyretype) det siste året, så
    # blir kriterie_ingen_1yr_prom_siste_aar = "nei".
    # NB - husk å gruppere på mceid (én prosedyre kan ha flere tidligere prom)
    fullmatch <- dplyr::inner_join(
      x = df_sub,
      y = proms_1yr,

      by = c("patient_id", "forlopstype" = "parent_mcetype"),

      relationship = "many-to-many") |>

      dplyr::filter(mceid_followup1 != mceid_followup1yr_oppf |
                      !mceid_followup1 %in% mceid_followup1yr_oppf) |>
      dplyr::mutate(
        teoretisk_prom_lower = dato_pros,
        teoretisk_prom_upper = dato_followup_teoretisk_1aar,
        kriterie_ingen_1yr_prom_siste_aar = ifelse(
          eprom_bestilt_dato > teoretisk_prom_lower &
            eprom_bestilt_dato <= teoretisk_prom_upper,
          "nei", NA_character_))



    return(
      dplyr::left_join(
        df,
        fullmatch %>%
          dplyr::group_by(mceid)  %>%
          dplyr::mutate(

            kriterie_ingen_1yr_prom_siste_aar = ifelse(
              any(kriterie_ingen_1yr_prom_siste_aar == "nei"),
              "nei",
              NA_character_),

            n = 1:dplyr::n())  %>%
          dplyr::ungroup() %>%

          dplyr::filter(n == 1) %>%
          dplyr::select(patient_id , mceid, kriterie_ingen_1yr_prom_siste_aar),
        by = c("patient_id", "mceid"))
    )
  }
  if(nrow(df_sub) == 0) {
    return(df  %>%  dplyr::mutate(kriterie_ingen_1yr_prom_siste_aar = NA_character_))
  }
}
#' @rdname prom_funksjoner
#' @export
kriterie_ingen_ny_pros_neste_aar <- function(df,
                                             reshId = reshId,
                                             userRole = userRole){

  stopifnot(c("kriterie_1yr_midlertidig",
              "dato_followup_teoretisk_1aar") %in% names(df))

  # alle PID for pasienter som har de øvrige kriteriene for 1-års-prom
  df_sub <- df  %>%
    dplyr::filter(kriterie_1yr_midlertidig %in% "ja") %>%
    dplyr::select(patient_id, dato_pros, mceid, forlopstype, dato_followup_teoretisk_1aar)

  if(nrow(df_sub)>0){

    list_patients <- df_sub  %>%
      dplyr::distinct(patient_id)  %>%
      dplyr::pull(patient_id)  %>%
      stringr::str_flatten_comma(last = ", ")

    # Alle prosedyrer for disse pasientene
    query <- paste0("
       SELECT
         pros.CENTREID,
         pros.MCEID AS mceid_ny,
         pros.FORLOPSTYPE AS forlopstype,
         pros.DATO_PROS AS dato_pros_ny,
         mce.PATIENT_ID AS patient_id
      FROM pros
      LEFT JOIN mce ON mce.MCEID = pros.MCEID
      WHERE pros.FORLOPSTYPE IN ('1', '2', '3', '4')
      AND mce.PATIENT_ID IN ",
                    "(", list_patients, ")",
                    " ;")

    nye_pros <- rapbase::loadRegData(
      registryName = "data",
      query = query)

    fullmatch <- dplyr::inner_join(
      x = df_sub,
      y = nye_pros,
      by = c("patient_id", "forlopstype"),
      relationship = "many-to-many") %>%
      dplyr::filter(dato_pros <= dato_pros_ny,
                    mceid != mceid_ny)%>%
      dplyr::mutate(
        teoretisk_prom_lower = dato_pros,
        teoretisk_prom_upper = dato_followup_teoretisk_1aar,
        kriterie_ingen_ny_pros = ifelse(
          (dato_pros_ny > teoretisk_prom_lower &
            dato_pros_ny <= teoretisk_prom_upper) |
            (dato_pros_ny == dato_pros &
               mceid < mceid_ny),
          "nei", NA_character_))



    return(
      dplyr::left_join(
        df,
        fullmatch %>%
          dplyr::group_by(mceid)  %>%
          dplyr::mutate(

            kriterie_ingen_ny_pros = ifelse(
              any(kriterie_ingen_ny_pros == "nei"),
              "nei",
              NA_character_),

            n = 1:dplyr::n())  %>%
          dplyr::ungroup() %>%

          dplyr::filter(n == 1) %>%
          dplyr::select(patient_id , mceid, kriterie_ingen_ny_pros),
        by = c("patient_id", "mceid"))
    )
  }
  if(nrow(df_sub) == 0) {
    return(df  %>%  dplyr::mutate(kriterie_ingen_ny_pros = NA_character_))
  }
}
#' @rdname prom_funksjoner
#' @export
kriterie_5yr_prom_siste_5aar <- function(df,
                                        reshId = reshId,
                                        userRole = userRole){

  stopifnot(c("kriterie_5yr_midlertidig",
              "dato_followup_teoretisk_5aar") %in% names(df))

  # alle PID for pasienter som har de øvrige kriteriene for 5-års-prom
  # og som har prosedyredato etter prodsetting av  5-års-prom
  df_sub <- df  %>%
    dplyr::filter(kriterie_5yr_midlertidig %in% "ja") %>%
    dplyr::select(patient_id, dato_pros, mceid, mceid_followup5,
                  forlopstype, dato_followup_teoretisk_5aar)

  if(nrow(df_sub)>0){

    list_patients <- df_sub  %>%
      dplyr::distinct(patient_id)  %>%
      dplyr::pull(patient_id)  %>%
      stringr::str_flatten_comma(last = ", ")

    # Finne alle 5års-prom for de utvalgte pasientene,
    # Legg til FORLOPSTYPE for prosedyren
    query = paste0("
       WITH
       prom5yr AS (
           SELECT
              F.`MCEID` as mceid_followup5yr_oppf,
              mce.`PATIENT_ID` AS patient_id,
              proms.`TSSENDT` as eprom_bestilt_dato,
              mce.`PARENTMCEID` as parentmceid_oppf
           FROM fiveyearfollowup F
           LEFT JOIN mce ON mce.MCEID = F.MCEID AND MCETYPE = 10
           LEFT JOIN proms ON proms.`MCEID` = F.MCEID and proms.REGISTRATION_TYPE = 'Fiveyearfollowup'
           ),
       main AS (
           SELECT
               mce.MCEID as parentmceid_oppf,
               mce.MCETYPE as parent_mcetype
           FROM mce
           WHERE mce.MCETYPE IN (1, 2, 3, 4)
       )

       SELECT
          prom5yr.mceid_followup5yr_oppf,
          prom5yr.patient_id,
          prom5yr.eprom_bestilt_dato,
          prom5yr.parentmceid_oppf,
          main.parent_mcetype
       FROM
         prom5yr
       LEFT JOIN
         main ON main.parentmceid_oppf = prom5yr.parentmceid_oppf
       WHERE
         prom5yr.patient_id IN ",
                   "(", list_patients, ")",
                   " ;")
    proms_5yr <- rapbase::loadRegData(
      registryName = "data",
      query = query)

    # finne ALLE oppfølginer av samme forløptype som prosedyren,
    #  som har bestillingsdato for eprom FØR prosedyre og som ikke hører
    #  direkte til denne prosedyren
    # Dersom eprom var bestilt (av samme prosedyretype) de siste 5 årene, så
    # blir kriterie_ingen_5yr_prom_siste_aar = "nei".
    # NB - husk å gruppere på mceid (én prosedyre kan ha flere tidligere prom)
    fullmatch <- dplyr::inner_join(
      x = df_sub,
      y = proms_5yr,

      by = c("patient_id", "forlopstype" = "parent_mcetype"),

      relationship = "many-to-many") |>

      dplyr::filter(mceid_followup5 != mceid_followup5yr_oppf |
                      !mceid_followup5 %in% mceid_followup5yr_oppf) |>
      dplyr::mutate(
        teoretisk_prom_lower = dato_pros,
        teoretisk_prom_upper = dato_followup_teoretisk_5aar,
        kriterie_ingen_5yr_prom_siste_5aar = ifelse(
          eprom_bestilt_dato > teoretisk_prom_lower &
            eprom_bestilt_dato <= teoretisk_prom_upper,
          "nei", NA_character_))

    return(
      dplyr::left_join(
        df,
        fullmatch %>%
          dplyr::group_by(mceid)  %>%
          dplyr::mutate(

            kriterie_ingen_5yr_prom_siste_5aar = ifelse(
              any(kriterie_ingen_5yr_prom_siste_5aar == "nei"),
              "nei",
              NA_character_),

            n = 1:dplyr::n())  %>%
          dplyr::ungroup() %>%

          dplyr::filter(n == 1) %>%
          dplyr::select(patient_id , mceid, kriterie_ingen_5yr_prom_siste_5aar),
        by = c("patient_id", "mceid"))
    )
  }
  if(nrow(df_sub) == 0) {
    return(df  %>%  dplyr::mutate(kriterie_ingen_5yr_prom_siste_5aar = NA_character_))
  }
}
#' @rdname prom_funksjoner
#' @export
kriterie_ingen_ny_pros_neste_5aar <- function(df,
                                             reshId = reshId,
                                             userRole = userRole){

  stopifnot(c("kriterie_5yr_midlertidig",
              "dato_followup_teoretisk_5aar") %in% names(df))

  # alle PID for pasienter som har de øvrige kriteriene for 1-års-prom
  df_sub <- df  %>%
    dplyr::filter(kriterie_5yr_midlertidig %in% "ja") %>%
    dplyr::select(patient_id, dato_pros, mceid, forlopstype, dato_followup_teoretisk_5aar)

  if(nrow(df_sub)>0){

    list_patients <- df_sub  %>%
      dplyr::distinct(patient_id)  %>%
      dplyr::pull(patient_id)  %>%
      stringr::str_flatten_comma(last = ", ")

    # Alle prosedyrer for disse pasientene
    query <- paste0("
       SELECT
         pros.CENTREID,
         pros.MCEID AS mceid_ny,
         pros.FORLOPSTYPE AS forlopstype,
         pros.DATO_PROS AS dato_pros_ny,
         mce.PATIENT_ID AS patient_id
      FROM pros
      LEFT JOIN mce ON mce.MCEID = pros.MCEID
      WHERE pros.FORLOPSTYPE IN ('1', '2', '3', '4')
      AND mce.PATIENT_ID IN ",
                    "(", list_patients, ")",
                    " ;")

    nye_pros <- rapbase::loadRegData(
      registryName = "data",
      query = query)

    fullmatch <- dplyr::inner_join(
      x = df_sub,
      y = nye_pros,
      by = c("patient_id", "forlopstype"),
      relationship = "many-to-many") %>%
      dplyr::filter(dato_pros <= dato_pros_ny,
                    mceid != mceid_ny)%>%
      dplyr::mutate(
        teoretisk_prom_lower = dato_pros,
        teoretisk_prom_upper = dato_followup_teoretisk_5aar,
        kriterie_ingen_ny_pros_neste_5aar = ifelse(
          (dato_pros_ny > teoretisk_prom_lower &
             dato_pros_ny <= teoretisk_prom_upper) |
            (dato_pros_ny == dato_pros &
               mceid < mceid_ny),
          "nei", NA_character_))



    return(
      dplyr::left_join(
        df,
        fullmatch %>%
          dplyr::group_by(mceid)  %>%
          dplyr::mutate(

            kriterie_ingen_ny_pros_neste_5aar = ifelse(
              any(kriterie_ingen_ny_pros_neste_5aar == "nei"),
              "nei",
              NA_character_),

            n = 1:dplyr::n())  %>%
          dplyr::ungroup() %>%

          dplyr::filter(n == 1) %>%
          dplyr::select(patient_id , mceid, kriterie_ingen_ny_pros_neste_5aar),
        by = c("patient_id", "mceid"))
    )
  }
  if(nrow(df_sub) == 0) {
    return(df  %>%  dplyr::mutate(kriterie_ingen_ny_pros_neste_5aar = NA_character_))
  }
}
