test_that("kodebok_sjekk_foer_leggtil fungerer", {


  df <- data.frame(var1 = c(1:5, 1:5),
                   var2 = rep(c(0, 1), 5),
                   var3 = c(1, 1, 1, 1, 1, 2, 2, 2, 3, NA),
                   var4 = rep(1, 10),
                   var4_tekst = rep("gammel tekst", 10),
                   var5 = c(rep(1, 8), NA, NA),
                   var6 = rep(NA, 10),
                   var7 = rep(1, 10),
                   var8 = rep(1, 10))

  kb <- data.frame(fysisk_feltnavn = c(rep("var1", 5),
                                       "var2", "var2",
                                       "var3", "var3",
                                       "var4",
                                       "var5", "var5",
                                       "var6", "var6",
                                       "var7", "var7",
                                       "var8", "var8"),
                   type = rep("Listevariabel", 18),
                   listeverdier = c(1:5,
                                    0, 1,
                                    1, 2,
                                    1,
                                    1, 2,
                                    5:6,
                                    1, 1,
                                    1, 2),
                   listetekst = c(letters[1:5],
                                  "nei", "ja",
                                  "mann", "kvinne",
                                  "Dette er nytt!",
                                  "litt", "mye",
                                  "verdi1", "verdi2",
                                  "verdi1", "verdi2",
                                  "2 ganger likt", "2 ganger likt"))

  # Foreventer at FALSE dersom nytt variabelnavn allerede finnes i df
  testthat::expect_false(
    ablanor::kodebok_sjekk_foer_leggtil(
      df = df,
      tekst_variabel = "var4_tekst",
      verdi_variabel = "var4",
      koder = kb %>% dplyr::filter(fysisk_feltnavn == "var4")))

  # Foreventer at FALSE dersom nivåer i df ikke finnes i kb
  testthat::expect_false(
    ablanor::kodebok_sjekk_foer_leggtil(
      df = df,
      tekst_variabel = "var3_tekst",
      verdi_variabel = "var3",
      koder = kb %>% dplyr::filter(fysisk_feltnavn == "var3")))


  # Foreventer at FALSE dersom kb har duplikater
  testthat::expect_false(
    ablanor::kodebok_sjekk_foer_leggtil(
      df = df,
      tekst_variabel = "var7_tekst",
      verdi_variabel = "var7",
      koder = kb %>% dplyr::filter(fysisk_feltnavn == "var7")))

  testthat::expect_false(
    ablanor::kodebok_sjekk_foer_leggtil(
      df = df,
      tekst_variabel = "var8_tekst",
      verdi_variabel = "var8",
      koder = kb %>% dplyr::filter(fysisk_feltnavn == "var8")))


  # Forventer feilmelding
  testthat::expect_error(
    ablanor::kodebok_fyll_listetekstvar(
      df = df,
      kb = kb %>% dplyr::select(-fysisk_feltnavn)))


})





testthat::test_that("kodebok_sjekk_foer_fjerning fungerer for Listevariabel", {



  df <- data.frame(var1 = c(1:5),
                   var1_tekst = letters[1:5],
                   var2 = c(1, 2, 1, 2, 1),
                   var2_tekst = c("ja", "nei", "ja", "nei", "ja"),
                   var3 = c(0, 0, 1, 1, 1),
                   var3_tekst = c("nei", "nei", "ja", "ja", "ja"),
                   var4_tekst = c("nei", "nei", "ja", "ja", "ja"))

  kb <- data.frame(fysisk_feltnavn = c(rep("var1", 5),
                                       "var2", "var2",
                                       "var3", "var3",
                                       "var4", "var4"),
                   type = rep("Listevariabel", 11),
                   listeverdier = c(1:5,
                                    1, 2,
                                    0, 1,
                                    0, 1),
                   listetekst = c(letters[1:5],
                                  "mann", "kvinne",
                                  "nei", "ja",
                                  "nei", "ja"))

  # Forventer TRUE fordi verdiene overstemmer
  testthat::expect_true(ablanor::kodebok_sjekk_foer_fjerning(
    df = df,
    kb = kb,
    verdi_variabel = "var1",
    tekst_variabel = "var1_tekst",
    type = "Listevariabel"))

  # forventer FALSE fordi var2_tekst er ulik verdiene i var2
  testthat::expect_false(ablanor::kodebok_sjekk_foer_fjerning(
    df = df,
    kb = kb,
    verdi_variabel = "var2",
    tekst_variabel = "var2_tekst",
    type = "Listevariabel"))


  # forventer TRUE fordi kb og df overstemmer
  testthat::expect_true(ablanor::kodebok_sjekk_foer_fjerning(
    df = df,
    kb = kb,
    verdi_variabel = "var3",
    tekst_variabel = "var3_tekst",
    type = "Listevariabel"))


  # Forventer FALSE fordi var4 mangler
  testthat::expect_false(ablanor::kodebok_sjekk_foer_fjerning(
    df = df,
    kb = kb,
    verdi_variabel = "var4",
    tekst_variabel = "var4_tekst",
    type = "Listevariabel"))


})

testthat::test_that("skjekk foer fjerning fungerer for avkrysninsboks", {

  # forventer at fungerer for binære avkrysningsbokser
  df <- data.frame(var1 = rep(0, 5),
                   var1_tekst = rep("nei", 5),
                   var2 = rep(1, 5),
                   var3 = rep(NA, 5),
                   var3_tekst = rep(NA_character_, 5),
                   var4 = c(0, 1, 0, 1, NA),
                   var4_tekst = c("nei", "ja", "nei", "ja", NA_character_),
                   var5 = 1:5,
                   var2_tekst = rep("finnes allerede", 5))

  kb <- data.frame(fysisk_feltnavn = paste0("var", 1:5),
                   type = rep("Avkrysningsboks", 5),
                   listeverdier = rep(NA, 5),
                   listetekst = rep(NA, 5))

  testthat::expect_true(ablanor::kodebok_sjekk_foer_fjerning(
    df = df,
    kb = kb,
    verdi_variabel = "var1",
    tekst_variabel = "var1_tekst",
    type = "Avkrysningsboks"))

  testthat::expect_false(ablanor::kodebok_sjekk_foer_fjerning(
    df = df,
    kb = kb,
    verdi_variabel = "var2",
    tekst_variabel = "var2_tekst",
    type = "Avkrysningsboks"))

  testthat::expect_true(ablanor::kodebok_sjekk_foer_fjerning(
    df = df,
    kb = kb,
    verdi_variabel = "var3",
    tekst_variabel = "var3_tekst",
    type = "Avkrysningsboks"))

  testthat::expect_true(ablanor::kodebok_sjekk_foer_fjerning(
    df = df,
    kb = kb,
    verdi_variabel = "var4",
    tekst_variabel = "var4_tekst",
    type = "Avkrysningsboks"))

  testthat::expect_false(ablanor::kodebok_sjekk_foer_fjerning(
    df = df,
    kb = kb,
    verdi_variabel = "var5",
    tekst_variabel = "var5_tekst",
    type = "Avkrysningsboks"))

})




test_that("kodebok_fyll_listetekstvar works", {

  df <- data.frame(var1 = c(1:5, 1:5),
                   var2 = rep(c(0, 1), 5),
                   var3 = c(1, 1, 1, 1, 1, 2, 2, 2, 3, NA),
                   var4 = rep(1, 10),
                   var4_tekst = rep("gammel tekst", 10),
                   var5 = c(rep(1, 8), NA, NA),
                   var6 = rep(NA, 10))

  kb <- data.frame(fysisk_feltnavn = c(rep("var1", 5),
                                       "var2", "var2",
                                       "var3", "var3",
                                       "var4",
                                       "var5", "var5",
                                       "var6", "var6"),
                   type = rep("Listevariabel", 14),
                   listeverdier = c(1:5,
                                    0, 1,
                                    1, 2,
                                    1,
                                    1, 2,
                                    5:6),
                   listetekst = c(letters[1:5],
                                  "nei", "ja",
                                  "mann", "kvinne",
                                  "Dette er nytt!",
                                  "litt", "mye",
                                  "verdi1", "verdi2"))


  # Forventer at autofyll for alle fungerer:
  testthat::expect_equal(
    ablanor::kodebok_fyll_listetekstvar(df = df,
                                        kb = kb,
                                        suffiks = "_tekst") %>%
      names(),
    c("var1", "var1_tekst", "var2", "var2_tekst",
      "var3", "var4", "var4_tekst", "var5", "var5_tekst",
      "var6", "var6_tekst"))

  # Forventer at vi får bare en ny her:
  testthat::expect_equal(
    ablanor::kodebok_fyll_listetekstvar(df = df, kb, var1) %>% names(),
    c("var1", "var1_tekst", "var2",
      "var3", "var4", "var4_tekst", "var5", "var6"))


  # Forventer at factor fungerer:
  testthat::expect_true(all(
    ablanor::kodebok_fyll_listetekstvar(df = df, kb) %>%
      dplyr::filter(var2 == 0) %>%
      dplyr::pull(var2_tekst) == "nei"))

  testthat::expect_true(all(
    ablanor::kodebok_fyll_listetekstvar(df = df, kb) %>%
      dplyr::filter(var2 == 1) %>%
      dplyr::pull(var2_tekst) == "ja"))


  # Forventer IKKE overskrevert variabel var4_tekst som fantes allerede
  testthat::expect_true(all(
    ablanor::kodebok_fyll_listetekstvar(df = df, kb, var4) %>%
      dplyr::pull(var4_tekst) == "gammel tekst"))


  # Forventer at NA ikke blir fylt på med annen verdi i tekst
  testthat::expect_true(all(
    ablanor::kodebok_fyll_listetekstvar(df = df, kb) %>%
      dplyr::filter(is.na(var5)) %>%
      dplyr::pull(var5_tekst) %>%
      is.na()))

  # Forventer bare NA dersom ingen match
  testthat::expect_true(all(
    ablanor::kodebok_fyll_listetekstvar(df = df, kb, var6) %>%
      dplyr::pull(var6_tekst) %>%
      is.na()))
})



test_that("Tekst for avkrysningsboks virker", {
  #  Tester for Avkrysningsboks
  df <- data.frame(var1 = rep(0, 5),
                   var2 = rep(1, 5),
                   var3 = rep(NA, 5),
                   var4 = c(0, 1, 0, 1, NA),
                   var5 = 1:5,
                   var2_tekst = rep("finnes allerede", 5))

  kb <- data.frame(fysisk_feltnavn = paste0("var", 1:5),
                   type = rep("Avkrysningsboks", 5),
                   listeverdier = rep(NA, 5),
                   listetekst = rep(NA, 5))

  testthat::expect_equal(
    ablanor::kodebok_fyll_avkrysningsboks(
      df = df,
      kb = kb,
      suffiks = "_tekst") %>%
      names(),
    c("var1", "var1_tekst", "var2", "var3", "var3_tekst",
      "var4", "var4_tekst", "var5", "var2_tekst"))

  testthat::expect_equal(
    ablanor::kodebok_fyll_avkrysningsboks(
      df = df,
      kb = kb,
      var1,
      suffiks = "_tekst") %>%
      names(),
    c("var1", "var1_tekst", "var2", "var3", "var4", "var5", "var2_tekst"))


  testthat::expect_true(all(
    ablanor::kodebok_fyll_avkrysningsboks(df = df,
                                          kb = kb,
                                          var2,
                                          suffiks = "_tekst") %>%
      dplyr::pull(var2_tekst) == "finnes allerede"))


  testthat::expect_true(all(
    ablanor::kodebok_fyll_avkrysningsboks(df = df,
                                          kb = kb,
                                          var1,
                                          suffiks = "_tekst") %>%
      dplyr::pull(var1_tekst) == "nei"))

  testthat::expect_true(all(
    ablanor::kodebok_fyll_avkrysningsboks(df = df,
                                          kb = kb,
                                          var3,
                                          suffiks = "_tekst") %>%
      dplyr::pull(var3_tekst) %>%
      is.na()))

  testthat::expect_true(all(
    ablanor::kodebok_fyll_avkrysningsboks(df = df,
                                          kb = kb,
                                          var4,
                                          suffiks = "_tekst") %>%
      dplyr::filter(!is.na(var4_tekst)) %>%
      dplyr::pull(var4_tekst) ==
      c("nei", "ja", "nei", "ja")))


  testthat::expect_error(
    ablanor::kodebok_fyll_avkrysningsboks(df = df)
  )

  testthat::expect_error(
    ablanor::kodebok_fyll_avkrysningsboks(kb = kb)
  )


})



test_that("kodebok_beholde_bare_listetekstvar fungerer", {
  df <- data.frame(var1 = c(1:5),
                   var1_tekst = letters[1:5],
                   var2 = c(1, 2, 1, 2, 1),
                   var2_tekst = c("ja", "nei", "ja", "nei", "ja"),
                   var3 = c(0, 0, 1, 1, 1),
                   var3_tekst = c("nei", "nei", "ja", "ja", "ja"),
                   var4_tekst = c("nei", "nei", "ja", "ja", "ja"))

  kb <- data.frame(fysisk_feltnavn = c(rep("var1", 5),
                                       "var2", "var2",
                                       "var3", "var3",
                                       "var4", "var4"),
                   type = rep("Listevariabel", 11),
                   listeverdier = c(1:5,
                                    1, 2,
                                    0, 1,
                                    0, 1),
                   listetekst = c(letters[1:5],
                                  "mann", "kvinne",
                                  "nei", "ja",
                                  "nei", "ja"))


  # Fungerer for fjerning av alle variablene:
  testthat::expect_equal(
    ablanor::kodebok_beholde_bare_listetekstvar(
      df = df,
      kb = kb,
      suffiks = "_tekst",
      fjerne_suffiks_fra_navn = TRUE) %>%
      names(),

    c("var1", "var2", "var2_tekst", "var3", "var4_tekst"))


  # Fungerer for fjerning av en variabel
  testthat::expect_equal(
    ablanor::kodebok_beholde_bare_listetekstvar(
      df = df,
      kb = kb,
      var1,
      suffiks = "_tekst",
      fjerne_suffiks_fra_navn = TRUE) %>%
      names(),

    c("var1", "var2", "var2_tekst", "var3", "var3_tekst", "var4_tekst"))


  # Omdøpe uten suffiks fungerer:
  testthat::expect_equal(
    ablanor::kodebok_beholde_bare_listetekstvar(
      df = df %>% dplyr::select("var1", "var1_tekst"),
      kb = kb,
      suffiks = "_tekst",
      fjerne_suffiks_fra_navn = TRUE) %>%
      names(),
    "var1")

  # Default verdier av funksjon fungerer
  testthat::expect_equal(
    ablanor::kodebok_beholde_bare_listetekstvar(
      df = df %>% dplyr::select("var1", "var1_tekst"),
      kb = kb) %>%
      names(),
    "var1")

  # Innhodet i nye variabler er riktig
  testthat::expect_equal(
    ablanor::kodebok_beholde_bare_listetekstvar(
      df = df %>% dplyr::select("var1", "var1_tekst"),
      kb = kb) %>%
      dplyr::pull(),
    letters[1:5])

  testthat::expect_equal(
    ablanor::kodebok_beholde_bare_listetekstvar(df = df,
                                                kb = kb) %>%
      dplyr::pull(var2),
    c(1, 2, 1, 2, 1))

  testthat::expect_equal(
    ablanor::kodebok_beholde_bare_listetekstvar(df = df,
                                                kb = kb) %>%
      dplyr::pull(var2_tekst),
    c("ja", "nei", "ja", "nei", "ja"))

  testthat::expect_equal(
    ablanor::kodebok_beholde_bare_listetekstvar(df = df,
                                                kb = kb) %>%
      dplyr::pull(var3),
    c("nei", "nei", "ja", "ja", "ja"))

  testthat::expect_equal(
    ablanor::kodebok_beholde_bare_listetekstvar(df = df,
                                                kb = kb) %>%
      dplyr::pull(var4_tekst),
    c("nei", "nei", "ja", "ja", "ja"))



  # Foreventer uforandret  dersom ingen variabler har suffikset;
  df <- data.frame(var2 = 1:5,
                   var3 = c("nei", "nei", "ja", "ja", "ja"))

  testthat::expect_equal(
    ablanor::kodebok_beholde_bare_listetekstvar(df, kb),
    df
  )


  # forventer at fungerer for binære avkrysningsbokser
  df <- data.frame(var1 = rep(0, 5),
                   var1_tekst = rep("nei", 5),
                   var2 = rep(1, 5),
                   var3 = rep(NA, 5),
                   var3_tekst = rep(NA_character_, 5),
                   var4 = c(0, 1, 0, 1, NA),
                   var4_tekst = c("nei", "ja", "nei", "ja", NA_character_),
                   var5 = 1:5,
                   var2_tekst = rep("finnes allerede", 5))

  kb <- data.frame(fysisk_feltnavn = paste0("var", 1:5),
                   type = rep("Avkrysningsboks", 5),
                   listeverdier = rep(NA, 5),
                   listetekst = rep(NA, 5))
  #


  testthat::expect_equal(
    ablanor::kodebok_beholde_bare_listetekstvar(df = df,
                                                kb = kb,
                                                suffiks = "_tekst") %>%
      names(),
    c("var1", "var2", "var3", "var4", "var5", "var2_tekst"))


  testthat::expect_equal(
    ablanor::kodebok_beholde_bare_listetekstvar(df = df,
                                                kb = kb,
                                                suffiks = "_tekst",
                                                var1) %>%
      names(),
    c("var1", "var2", "var3", "var3_tekst", "var4",
      "var4_tekst", "var5", "var2_tekst"))

  testthat::expect_error(
    ablanor::kodebok_beholde_bare_listetekstvar(kb = kb,
                                                suffiks = "_tekst", var1)
  )

})


test_that("kjeden av kodebok-funksjoner fungerer", {

  . <- ""
  df <- data.frame(var1 = 1:5,
                   var2 = rep(1, 5),
                   var3 = c(NA, 0, 1, 0, 1),
                   var4 = c(0, 1, 0, 1, NA),
                   var5 = 1:5,
                   var6 = c(0, 1, 2, 0, 1),
                   var7 = c(0, 1, 0, 1, 1),
                   var2_tekst = rep("finnes allerede", 5))

  kb <- data.frame(fysisk_feltnavn = c(rep("var1", 5),
                                       "var2", "var3", "var3",
                                       "var4",
                                       "var5", "var5", "var5",
                                       "var6", "var7"),
                   type = c(rep("Listevariabel", 5),
                            "Avkrysningsboks", rep("Listevariabel", 2),
                            "Annet",
                            rep("Listevariabel", 3),
                            "Avkrysningsboks", "Avkrysningsboks"),
                   listeverdier = c(1:5, NA, 0:1, NA, 1:3, NA, NA),
                   listetekst = c(letters[1:5], NA, "mann", "kvinne", NA,
                                  "ingenting", "litt", "mye", NA, NA))


  df_out <- df %>%
    ablanor::kodebok_fyll_listetekstvar(df = .,
                                        kb = kb,
                                        suffiks = "_tekst") %>%
    ablanor::kodebok_fyll_avkrysningsboks(df = .,
                                          kb = kb,
                                          suffiks = "_tekst") %>%
    ablanor::kodebok_beholde_bare_listetekstvar(df = .,
                                                kb = kb,
                                                suffiks = "_tekst",
                                                fjerne_suffiks_fra_navn = TRUE)

  testthat::expect_true(all(
    df_out %>%
      dplyr::select(var1) == letters[1:5]))

  testthat::expect_true(all(
    df_out %>%
      dplyr::select(var2) == rep(1, 5)))

  testthat::expect_true(all(
    df_out %>%
      dplyr::select(var3) %>%
      dplyr::filter(!is.na(var3)) ==
      c("mann", "kvinne", "mann", "kvinne")))

  testthat::expect_true(all(
    df_out %>%
      dplyr::select(var4) %>%
      dplyr::filter(!is.na(var4)) == c(0, 1, 0, 1)))

  testthat::expect_true(all(
    df_out %>%
      dplyr::select(var5) == 1:5))


  testthat::expect_true(all(
    df_out %>%
      dplyr::select(var6) == c(0:2, 0:1)))

  testthat::expect_true(all(
    df_out %>%
      dplyr::select(var7) == c("nei", "ja", "nei", "ja", "ja")))

  testthat::expect_true(all(
    df_out %>%
      dplyr::select(var2_tekst) == rep("finnes allerede", 5)))


})
