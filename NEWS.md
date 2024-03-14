# ablanor 1.3.2
Fjerne egne getData for sammenstilte skjemaer i utforsker. All 
databehandling skjer nå i getPrep funksjonene. Lagt til flere variabler fra
basis og prosedyreskjemaet. Enklere å legge til/fjerne variabler.


# ablanor 1.3.1
Sammenstille prosedyreskjema med Eproms ved basis, 
1 år og 5 år, samt hendelser. svarprosent.
Ny kodebok og tilpasset dupliserte feltnavn til de ulike tabellene. 

# ablanor 1.3.0
Ny release av innregistrering med nye funksjonaliteter. Eproms ved basis, 
1 år og 5 år, samt hendelser i egen tabell. Tilpasse Rapporteket til dette. 

## Nytt
* Rådata i utforsker for alle tabeller
* Nedlasting av proms-status, hendelse og basis
* samle alle getData
* samle alle getPrepData
* getDataDump bruker getData


## Bugfix
* Nedlasting av followup uten datofilter (status -1 forsvant før)

# ablanor 1.2.0
Små justeringer på Rapporteket etter overgang til FALK som innloggings-
portal i prod. 
To nye reshid. 
Bruker egendefinerte (finere) sykehusnavn i Rapporteket og i rapporter. 

# ablanor 1.1.1

* Installer Rapbase fra github i stedet for cran. Rapbase ble fjernet fra Cran i slutten av juni 2023 fordi en test feilet. Testen er fikset nå, men Rapbase er ikke publisert på nytt enda. Enn så lenge må pakken installeres direkte fra github.

# ablanor 1.1.0
## Nytt
* Nye versjoner av kvalitetsindikatorene. 
* Legge til indikatorene i utforsker 

## Bugfix
* Fjerne unødvendig bruk av _kodebok_fyll_ ved SINGLE-row-spørringer. Sparer
mye tid ved innlasting av kodebok og navn til variabelliste i utforsker

# ablanor 1.0.2
Bugfix: Ikke abonnere på veiledning + Fikse header slik at html ser bra ut
Ny parameter i legg_til_forlopstype_kortnavn

# ablanor 1.0.1
Bugfix: Do not add listetekst-vars before download of pros_patient_followup

# ablanor 1.0.0
First version ready for prod


# ablanor 0.5.0
* Added panel "Kodebok". Contains table with metadata ([#21](https://github.com/Rapporteket/ablanor/pull/21))



# ablanor 0.4.0
* added functions with tests for usual operations ([#17](https://github.com/Rapporteket/ablanor/pull/17))
* add followup data ([#17](https://github.com/Rapporteket/ablanor/pull/17))
* kodebok-functions to add  "listetekst"  for "listevariabler" and
"avkrysninsboks"  ([#14](https://github.com/Rapporteket/ablanor/pull/14))
* possibility to download merged table with patient, procedure and followup  ([#17](https://github.com/Rapporteket/ablanor/pull/17))
* User sorters in RpivptTable to order factor variables
* Cleanup in access for LC-role and SC-role



# ablanor 0.3.0

* Added (conceptual) page for report dispatch management using modules from rapbase ([#10](https://github.com/Rapporteket/ablanor/pull/10))
* Implemented server side restriction of module functionality and also implemented subscriptions by modules ([#11](https://github.com/Rapporteket/ablanor/pull/11))
* Added Klokeboken data as part of package ([#15](https://github.com/Rapporteket/ablanor/pull/15))

# ablanor 0.2.0

* Standard RMardown rendering performed by rapbase functions ([#7](https://github.com/Rapporteket/ablanor/pull/7))
* Applied temporal fix for converting variable names to lowercase (see [#6](https://github.com/Rapporteket/ablanor/issues/6))
* Added report allowing registry data export for statistical and development purposes ([#9](https://github.com/Rapporteket/ablanor/pull/9))
* Added some pimping of monthly report (that might not be acceptable)
* General code clean-up

# ablanor 0.1.1

* Fixed minor bugs relating to first try at Rapporteket ([#4](https://github.com/Rapporteket/ablanor/pull/4))

# ablanor 0.1.0

* Alot of changes and new code, alot of it still conceptual ([#3](https://github.com/Rapporteket/ablanor/pull/3))
* First version to be tried at Rapporteket

# ablanor 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
