
# TESTER for OVERLEVELSE
testthat::test_that("Ki overlevelse fungerer - sensur", {

  # dEL 1 UTLEDE SENSUR
  df <- data.frame(dato_pros = as.Date(c(NA, rep("2020-10-15", 6),
                                         "2021-09-20",
                                         "2022-10-15", "2021-10-15"),
                                       format = "%Y-%m-%d"),
                   deceased = c(0, NA, 1, 1, 1, 1, 0, 0, 0, 0),
                   deceased_date = as.Date(c(NA, NA, NA,
                                             "2020-05-20",
                                             "2020-10-15",
                                             "2020-11-10", NA, NA, NA, NA),
                                           format = "%Y-%m-%d"))

  df_out <- ablanor::utlede_dager_sensur(
    df,
    dato_sensur = as.Date("2021-10-20", format = "%Y-%m-%d"))


  # forventer ikke utregnet tidsdifferanse her:
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(is.na(.data$deceased_date),
                    .data$deceased %in% 1) %>%
      dplyr::pull(.data$dager_pros_sensur) %>%
      is.na()
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(is.na(.data$dato_pros)) %>%
      dplyr::pull(.data$dager_pros_sensur) %>%
      is.na()
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(is.na(.data$deceased)) %>%
      dplyr::pull(.data$dager_pros_sensur) %>%
      is.na()
  ))

  # Forventer disse tidsdifferansene her:
  testthat::expect_equal(
    df_out %>%
      dplyr::filter(!is.na(.data$dager_pros_sensur)) %>%
      dplyr::pull(.data$dager_pros_sensur),

    c(-148, 0, 26, 370, 30, -360, 5)
  )


  # Forventede verdier for dager_pros_sensur_yldig
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(is.na(.data$dager_pros_sensur )) %>%
      dplyr::pull(.data$dager_pros_sensur)  == "manglende"
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$dager_pros_sensur < 0) %>%
      dplyr::pull(.data$dager_pros_sensur)  == "nei"
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$dager_pros_sensur < 30,
                    .data$deceased %in% 0) %>%
      dplyr::pull(.data$dager_pros_sensur)  == "nei"
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$dager_pros_sensur >= 0,
                    .data$deceased %in% 1) %>%
      dplyr::pull(.data$dager_pros_sensur)  == "ja"
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$dager_pros_sensur >= 30,
                    .data$deceased %in% 0) %>%
      dplyr::pull(.data$dager_pros_sensur)  == "ja"
  ))

  # Forventer feil
  testthat::expect_error(
    ablanor::utlede_dager_sensur(df = data.frame(feil_navn = 1)))

  testthat::expect_error(
    ablanor::utlede_dager_sensur(df = df,
                                 dato_sensur = "ingen dato"))


})


testthat::test_that("Ki overlevelse fungerer - sensur", {

  # dEL 2 : Indikator - et forløp per pasient
  df <- data.frame(
    forlopstype = c(2:4, NA, rep(1, 11)),
    abla_strat_av_his = c(rep(0, 4), 1, rep(0, 10)),
    patient_id = c(1:15),
    deceased = c(rep(0, 10), rep(1, 5)),
    dager_pros_sensur_gyldig = c(rep("ja", 5), "nei", "manglende", rep("ja", 8)),
    dager_pros_sensur = c(rep(40, 5),29, NA, rep(40, 3), 28:32),
    dato_pros = rep(as.Date("2020-10-20", format = "%Y-%m-%d"), 15))


  df_out <- ablanor::indik_overlevelse30dg(df = df)

  # Forventer ikke i datagrunnlaget
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$forlopstype %in% 1) %>%
      dplyr::pull(.data$indik_overlevelse30dg_data) == "nei"
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$abla_strat_av_his %in% 0) %>%
      dplyr::pull(.data$indik_overlevelse30dg_data) == "nei"
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$dager_pros_sensur_gyldig  %in% "ja") %>%
      dplyr::pull(.data$indik_overlevelse30dg_data) == "nei"
  ))

  # Forventer i datagrunnlaget
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype %in% 1,
                    .data$abla_strat_av_his %in% 0,
                    .data$dager_pros_sensur_gyldig  %in% "ja") %>%
      dplyr::pull(.data$indik_overlevelse30dg_data) == "ja"
  ))

  # Forventert verdi av indikatoren
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_overlevelse30dg_data %in% "ja",
                    .data$deceased == 0) %>%
      dplyr::pull(.data$indik_overlevelse30dg) == "ja"
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_overlevelse30dg_data %in% "ja",
                    .data$deceased == 1,
                    .data$dager_pros_sensur >= 30) %>%
      dplyr::pull(.data$indik_overlevelse30dg) == "ja"
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_overlevelse30dg_data %in% "ja",
                    .data$deceased == 1,
                    .data$dager_pros_sensur < 30) %>%
      dplyr::pull(.data$indik_overlevelse30dg) == "nei"
  ))


})



testthat::test_that("Ki overlevelse fungerer - sensur", {

  # dEL 2 : Indikator - flere forløpe per pasient
  df <- data.frame(
    mceid = 1:10,
    forlopstype = rep(1, 10),
    abla_strat_av_his = rep(0, 10),
    patient_id = c(1, 1, 1, 2,  2, 3, 3, 3, 4, 4),
    deceased = c(rep(0, 5), rep(1, 5)),
    deceased_date = as.Date(c(NA, NA, NA,NA, NA,
                              rep("2020-12-24", 3),
                              rep("2021-12-01", 2)),
                            format = "%Y-%m-%d"),
    dato_pros = as.Date(c("2020-10-01",
                          "2020-10-02",
                          "2020-10-03",
                          "2020-10-01",
                          "2021-10-01",
                          "2019-12-24",
                          "2020-12-23",
                          "2020-12-24",
                          "2023-12-15",
                          "2021-10-01"),
                        format = "%Y-%m-%d"))

  df_out <- ablanor::utlede_dager_sensur(
    df = df,
    dato_sensur = as.Date("2020-10-31", format = "%Y-%m-%d")) %>%
    ablanor::indik_overlevelse30dg(.)



  # Disse forløpene er  ikke med i datagrunnlaget
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(! .data$mceid %in% c(1, 4, 6, 8, 10)) %>%
      dplyr::pull(.data$indik_overlevelse30dg_data) == "nei"
  ))

  # Disse forløpene er  med i datagrunnlaget
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$mceid %in% c(1, 4, 6, 8, 10)) %>%
      dplyr::pull(.data$indik_overlevelse30dg_data) == "ja"
  ))

  # Forventede verdier
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$mceid %in% c(1, 4, 6, 10)) %>%
      dplyr::pull(.data$indik_overlevelse30dg) == "ja"
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$mceid %in% c(8)) %>%
      dplyr::pull(.data$indik_overlevelse30dg) == "nei"
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_overlevelse30dg_data == "nei") %>%
      dplyr::pull(.data$indik_overlevelse30dg) %>%  is.na()
  ))

})



# Tester for TAMPONADE ----
test_that("KI-TAMPONADE wokss", {

  df <- data.frame(forlopstype = c(2, 3, 4, NA, 1, 1, 1, 1, 1),
                   abla_strat_av_his = c(NA, 1, 0, 0, 1, 0, 1, 0, 0),
                   komp_tamp = c(rep(0, 6), 1, 1, NA))
  df_out <- ablanor::indik_tamponade(df = df)


  # Forventer ikke i datagrunnlaget for disse:
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(! .data$forlopstype %in% 1) %>%
      dplyr::pull(.data$indik_tamp_data) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(! .data$abla_strat_av_his %in% 0) %>%
      dplyr::pull(.data$indik_tamp_data) == "nei"))


  # Forventer i datagrunnlaget for disse:
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype %in% 1,
                    .data$abla_strat_av_his %in% 0) %>%
      dplyr::pull(.data$indik_tamp_data) == "ja"))

  # Forventer ja/nei/manglende
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_tamp_data == "ja",
                    .data$komp_tamp == 0) %>%
      dplyr::pull(.data$indik_tamp) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_tamp_data == "ja",
                    .data$komp_tamp == 1) %>%
      dplyr::pull(.data$indik_tamp) == "ja"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_tamp_data == "ja",
                    is.na(.data$komp_tamp)) %>%
      dplyr::pull(.data$indik_tamp) == "manglende"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_tamp_data == "nei") %>%
      dplyr::pull(.data$indik_tamp) %>% is.na()))

  # Forventer feilmelding
  testthat::expect_error(
    ablanor::indik_tamponade(data.frame(forlopstype = 1,
                                        abla_strat_av_his =0))
  )


})






# Test indikator: klinisk effekt 12mnd etter -----
testthat::test_that("KI: Klinisk effekt fungerer", {
  df <- data.frame(
    forlopstype = c(2, 3, 4, 5, NA, rep(1, 10)),
    abla_strat_av_his = c(rep(0, 5), 1, rep(0, 9)),
    followup_status = c(rep(1, 6), NA, 0, -1, rep(1, 6)),
    q2 = c(rep(4, 6), NA, 1, NA, 1:5, 3)
  )

  df_out <- ablanor::indik_prom_klineff(df)

  # Forventer ikke i datagrunnlag
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$forlopstype %in% 1) %>%
      dplyr::pull(.data$indik_prom_klineff_data) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$abla_strat_av_his %in% 0) %>%
      dplyr::pull(.data$indik_prom_klineff_data) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(is.na(.data$followup_status)) %>%
      dplyr::pull(.data$indik_prom_klineff_data) == "nei"))


  # Forventer i datagrunnlaget
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!is.na(.data$followup_status),
                    .data$abla_strat_av_his %in% 0,
                    .data$forlopstype %in% 1) %>%
      dplyr::pull(.data$indik_prom_klineff_data) == "ja"))

  # Forventer at alle er enten ja eller nei (ingen NA)
  testthat::expect_equal(
    df_out %>%
      dplyr::count(.data$indik_prom_klineff_data) %>%
      dplyr::pull(.data$indik_prom_klineff_data),
    c("ja", "nei"))

  # Forventer at dersom ikke i datagrunnlaget er alle NA
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_prom_klineff_data %in% "nei") %>%
      dplyr::pull(.data$indik_prom_klineff) %>%
      is.na()))

  # Forventet utkomme dersom i datagrunnlaget
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_prom_klineff_data %in% "ja",
                    is.na(.data$q2)) %>%
      dplyr::pull(.data$indik_prom_klineff) == "manglende"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_prom_klineff_data %in% "ja",
                    .data$q2 %in% 1:3) %>%
      dplyr::pull(.data$indik_prom_klineff) == "ja"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_prom_klineff_data %in% "ja",
                    .data$q2 %in% 4:5) %>%
      dplyr::pull(.data$indik_prom_klineff) == "nei"))


  # Forventer feilmelding
  testthat::expect_error(
    ablanor::indik_prom_klineff(df = data.frame(forlopstpe = 1,
                                                abla_strat_av_his = 0,
                                                followup_status = NA))
  )
})




# Test indikator: AKutt suksess  ----
testthat::test_that("KI: Akutt suksess fungerer", {
  df <- data.frame(
    abla_strat_ingen = c(1, NA, rep(0, 18)),
    abla_strat_av_his = c(0, 0, 1, NA,  rep(0, 16)),
    forlopstype = c(rep(1, 4), NA, rep(1, 5), rep(2, 3), rep(3, 6), 4),
    aryt_i47_1_underkat = c(rep(NA, 13), NA, 1:5, NA),
    akutt_suksess = c(rep(NA, 5), NA, 9, 0, 1, 2, 0:2, 0:2, 0:2 , 1)
  )


  df_out <- ablanor::indik_akuttsuksess(df)

  # Forventer ikke i datagrunnlag
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$abla_strat_ingen %in% 0) %>%
      dplyr::pull(.data$indik_akuttsuksess_data) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$abla_strat_av_his %in% 0) %>%
      dplyr::pull(.data$indik_akuttsuksess_data) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(is.na(.data$forlopstype),
                    !.data$forlopstype %in% 1:3) %>%
      dplyr::pull(.data$indik_akuttsuksess_data) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype %in% 3,
                    !.data$aryt_i47_1_underkat %in% c(1, 2, 4)) %>%
      dplyr::pull(.data$indik_akuttsuksess_data) == "nei"))

  # Forventer i datagrunnlaget
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$abla_strat_ingen %in% 0,
                    .data$abla_strat_av_his %in% 0,
                    .data$forlopstype %in% 1) %>%
      dplyr::pull(.data$indik_akuttsuksess_data) == "AFLI"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$abla_strat_ingen %in% 0,
                    .data$abla_strat_av_his %in% 0,
                    .data$forlopstype %in% 2) %>%
      dplyr::pull(.data$indik_akuttsuksess_data) == "VT"))


  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$abla_strat_ingen %in% 0,
                    .data$abla_strat_av_his %in% 0,
                    .data$forlopstype %in% 3,
                    .data$aryt_i47_1_underkat %in% c(1, 2)) %>%
      dplyr::pull(.data$indik_akuttsuksess_data) == "AVNRT"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$abla_strat_ingen %in% 0,
                    .data$abla_strat_av_his %in% 0,
                    .data$forlopstype %in% 3,
                    .data$aryt_i47_1_underkat %in% 4) %>%
      dplyr::pull(.data$indik_akuttsuksess_data) == "AVRT"))

  # Forventer disse niv?ene (ingen NA)
  testthat::expect_true(all(
    df_out %>%
      dplyr::count(.data$indik_akuttsuksess_data) %>%
      dplyr::pull(.data$indik_akuttsuksess_data) %>%
      as.character() ==
      c("AFLI","VT", "AVRT", "AVNRT", "nei")))

  # Forventer at dersom ikke i datagrunnlaget er alle NA
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_akuttsuksess_data %in% "nei") %>%
      dplyr::pull(.data$indik_akuttsuksess) %>%
      is.na()))

  # Forventet utkomme dersom i datagrunnlaget
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$indik_akuttsuksess_data %in% "nei",
                    (is.na(.data$akutt_suksess) |
                       !.data$akutt_suksess %in% 0:2)) %>%
      dplyr::pull(.data$indik_akuttsuksess) == "manglende"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$indik_akuttsuksess_data %in% "nei",
                    .data$akutt_suksess %in% c(0, 2)) %>%
      dplyr::pull(.data$indik_akuttsuksess) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$indik_akuttsuksess_data %in% "nei",
                    .data$akutt_suksess %in% 1) %>%
      dplyr::pull(.data$indik_akuttsuksess) == "ja"))


  # Forventer feilmelding
  testthat::expect_error(
    ablanor::indik_akuttsuksess(df = data.frame(forlopstpe = 1,
                                                abla_strat_av_his = 0,
                                                akutt_suksess = NA))
  )
})


# Test indikator: Pacemakerbehov
testthat::test_that("KI: pacemakerbehov fungerer", {

  df <- data.frame(
    forlopstype = c(1, 2, 4, 9, NA, rep(3, 5)),
    abla_strat_av_his = c(rep(0, 5), 1, NA, rep(0, 3)),
    komp_avblokk_pm = c(rep(1, 7), NA, 0, 1)
  )

  df_out <- ablanor::indik_pacemaker(df)

  # Forventer ikke i datagrunnlaget
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$forlopstype %in% 3) %>%
      dplyr::pull(.data$indik_pacemaker_data) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$abla_strat_av_his %in% 0) %>%
      dplyr::pull(.data$indik_pacemaker_data) == "nei"))


  # Forventer i datagrunnlaget
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$abla_strat_av_his %in% 0,
                    .data$forlopstype %in% 3) %>%
      dplyr::pull(.data$indik_pacemaker_data) == "ja"))

  # Forventer disse verdiene i datagrunnlaget
  testthat::expect_equal(
    df_out %>%
      dplyr::count(.data$indik_pacemaker_data) %>%
      dplyr::pull(.data$indik_pacemaker_data),
    c("ja", "nei"))

  # Forventer NA for alle dersom NEI
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_pacemaker_data == "nei") %>%
      dplyr::pull(.data$indik_pacemaker) %>% is.na()))

  # Forventede verdier dersom JA
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_pacemaker_data == "ja",
                    .data$komp_avblokk_pm %in% 0) %>%
      dplyr::pull(.data$indik_pacemaker)== "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_pacemaker_data == "ja",
                    .data$komp_avblokk_pm %in% 1) %>%
      dplyr::pull(.data$indik_pacemaker)== "ja"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_pacemaker_data == "ja",
                    is.na(.data$komp_avblokk_pm)) %>%
      dplyr::pull(.data$indik_pacemaker) == "manglende"))

  # Forventer feilmelding
  testthat::expect_error(
    ablanor::indik_pacemaker(
      df = data.frame(tullevariabel_med_feil_navn = 1)))


})




# Tester for AVBRUDD -----
test_that("KI-AVBRUDD wokrs", {

  df <- data.frame(
    forlopstype = c(4, NA, rep(1, 7), 2, 3, 1, 2, 2, 2, 2),
    abla_strat_av_his = c (0, 0,  NA, rep(0, 6), NA, 1, 1, 0, 1, 0, 0),
    abla_strat_ingen = c(rep(0, 3), NA,  1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, NA),
    abla_strat_ingen_arsak = c(rep(NA, 4), 1, 4,5, rep(NA, 5), 4, NA, 99, NA))

  df_out <- ablanor::indik_avbrudd(df = df)


  # Forventer ikke i datagrunnlaget for disse:
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype %in% 4 | is.na(.data$forlopstype)) %>%
      dplyr::pull(.data$indik_avbrudd_data) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(
        .data$forlopstype == 1,
        is.na(.data$abla_strat_av_his)) %>%
      dplyr::pull(.data$indik_avbrudd_data) == "nei"))


  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(is.na(.data$abla_strat_ingen)) %>%
      dplyr::pull(.data$indik_avbrudd_data) == "nei"))



  # Forventer i datagrunnlaget for disse:
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype %in% 1,
                    .data$abla_strat_av_his %in% 0,
                    !is.na(.data$abla_strat_ingen)) %>%
      dplyr::pull(.data$indik_avbrudd_data) == "AFLI"))


  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype %in% 2:3,
                    !is.na(.data$abla_strat_ingen)) %>%
      dplyr::pull(.data$indik_avbrudd_data) == "VT, SVT, AV-knuter"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype %in% 1:3,
                    !is.na(.data$abla_strat_ingen),
                    .data$abla_strat_av_his %in% 1) %>%
      dplyr::pull(.data$indik_avbrudd_data) == "VT, SVT, AV-knuter"))



  # Forventer ja/nei/manglende
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$indik_avbrudd_data %in% "nei",
                    .data$abla_strat_ingen %in% 1,
                    .data$abla_strat_ingen_arsak %in% 4:5) %>%
      dplyr::pull(.data$indik_avbrudd) == "ja"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$indik_avbrudd_data %in% "nei",
                    .data$abla_strat_ingen %in% 0) %>%
      dplyr::pull(.data$indik_avbrudd) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$indik_avbrudd_data %in% "nei",
                    .data$abla_strat_ingen %in% 1,
                    .data$abla_strat_ingen_arsak %in% c(1, 2, 3, 9)) %>%
      dplyr::pull(.data$indik_avbrudd) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(!.data$indik_avbrudd_data %in% "nei",
                    .data$abla_strat_ingen %in% 1,
                    is.na(.data$abla_strat_ingen_arsak)) %>%
      dplyr::pull(.data$indik_avbrudd)  == "manglende"))


  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_avbrudd_data == "nei") %>%
      dplyr::pull(.data$indik_avbrudd) %>%  is.na()))


  # Forventer feilmelding
  testthat::expect_error(
    ablanor::indik_avbrudd(data.frame(forlopstype = 1,
                                      abla_strat_av_his =0))
  )


})

