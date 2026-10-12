#' Hjelpefunksjoner for PROM
#'

#' @param df Inneholder data fra pci + pciprom. Må inneholde
#' variabelen \code{kriterie_alle_midlertidig}.
#' @param registryName for SQL queries
#'
#' @name prom_funksjoner
#' @aliases kriterie_taviprom_siste_aar  kriterie_pciprom_siste_aar kriterie_ingen_tavi_neste3mnd
#'
#'
#' @rdname prom_funksjoner
#' @export
kriterie_basisprom_siste_aar <- function(df,
                                         reshId = reshId,
                                         userRole = userRole,
                                         fromDate = NULL,
                                         toDate = NULL){

  stopifnot(c("kriterie_basis_midlertidig") %in% names(df))

  # alle PID for pasienter med pci som har de øvrige kriteriene for pci-prom
  # og som har prosedyredato etter prodsetting av pci-prom
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

    # Dersom Proms basis er bestilt i vinduet
    # [12mnd FØR prosedyren; prosedato+1dag]
    # så blir kriterie_ingen_afli = nei
    fullmatch_afli <- inner_join(
      x = df_sub %>%
        dplyr::filter(dato_pros >= as.Date("2023-11-08", format = "%Y-%m-%d"),
                      forlopstype %in% 1) |>
        dplyr::select(-forlopstype, - kriterie_basis_midlertidig),

      y = proms_basis |>
        dplyr::filter(parent_mcetype %in% 1) |>
        dplyr::select(-parent_mcetype),

      by = c("patient_id"),
      relationship = "many-to-many") |>
      dplyr::filter(mceid_followupbasis != mceid_followupbasis_oppf |
                      !mceid_followupbasis %in% mceid_followupbasis_oppf,
                    dato_pros >= eprom_bestilt_dato) |>
      dplyr::filter(dato_pros >= as.Date("2022-11-08", format = "%Y-%m-%d")) |>  # 1-år før innføring av basis prom
      dplyr::mutate(
        teoretisk_prom_lower = dato_pros %m-% months(12),
        teoretisk_prom_upper = days(1),
        kriterie_ingen_afli = ifelse(
          eprom_bestilt_dato > teoretisk_prom_lower &
            eprom_bestilt_dato <= teoretisk_prom_upper,
          "nei", NA_character_))

    # Dersom Proms basis er bestilt i vinduet
    # [12mnd FØR prosedyren; prosedato+1dag]
    # så blir kriterie_ingen_afli = nei
    fullmatch <- inner_join(
      x = df_sub %>%
        dplyr::filter(dato_pros >= as.Date("2023-11-08", format = "%Y-%m-%d")) |>
        dplyr::select(- kriterie_basis_midlertidig),

      y = proms_basis,
      by = c("patient_id", "forlopstype" = "parent_mcetype"),
      relationship = "many-to-many") |>
      dplyr::filter(mceid_followupbasis != mceid_followupbasis_oppf |
                      !mceid_followupbasis %in% mceid_followupbasis_oppf,
                    dato_pros >= eprom_bestilt_dato) |>
      dplyr::filter(dato_pros >= as.Date("2022-11-08", format = "%Y-%m-%d")) |>  # 1-år før innføring av basis prom
      dplyr::mutate(
        teoretisk_prom_lower = dato_pros %m-% months(12),
        teoretisk_prom_upper = days(1),
        kriterie_ingen_basisprom_siste12mnd = ifelse(
          eprom_bestilt_dato > teoretisk_prom_lower &
            eprom_bestilt_dato <= teoretisk_prom_upper,
          "nei", NA_character_))



    return(
      dplyr::left_join(
        df,
        df_sub  %>%  dplyr::select(kriterie_ingen_taviprom, PasientID, ForlopsID),
        by = c("PasientID", "ForlopsID"))
    )
  }
  if(nrow(df_sub)== 0) {
    return(df  %>%  dplyr::mutate(kriterie_ingen_taviprom = NA_character_))
  }
}

