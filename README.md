# SHIC – Soil Health Indicators con logica fuzzy

Script, dati e mappe interattive della tesi sul calcolo dell'indice SHIC tramite sistemi di inferenza fuzzy (FIS).

## Contenuto

```
script/
  ISRIC_final.R                 analisi sui dati ISRIC/WoSIS scaricati via API
  Functions.R                   funzioni usate da ISRIC_final.R
  fuzzy_lastvL_modified_2.R     sistema fuzzy (input: phy.csv, che.csv, soc.csv, results.csv)
  phy.csv, che.csv, soc.csv, results.csv
dati_regioni/                   dati SOTWIS per regione (shapefile zippati)
  Europe_east.zip, Europe_west.zip, India.zip, Tunisie.zip, Centre_Afrique.zip, Amérique_latine.zip
mappe/
  mappa_shic_30cm.html          mappa SHIC sui primi 30 cm
  mappa_shic_orizzonti.html     mappa SHIC su tutti gli orizzonti
  dati_shic_orizzonti_1.js      dati della mappa orizzonti (prima metà)
  dati_shic_orizzonti_2.js      dati della mappa orizzonti (seconda metà)
```

## Dati da scaricare a parte: `data_api.RData`

Il file `data_api.RData` (circa 1,3 GB) supera il limite di GitHub per i file nella repository.
È allegato alla **Release** della repository: scaricalo dalla pagina *Releases* e mettilo nella cartella `script/`.

## Libreria `soilhub`

`ISRIC_final.R` usa la libreria `soilhub`: <https://github.com/giulange/soilhub-r>

```r
# install.packages("remotes")
remotes::install_github("giulange/soilhub-r")
```

Altri pacchetti richiesti: `sf`, `dplyr`, `FuzzyR`, `ggplot2`, `knitr`.

## Come eseguire

Gli script usano percorsi relativi: imposta `script/` come cartella di lavoro prima di eseguirli.

```r
setwd("percorso/della/repo/script")
source("ISRIC_final.R")
```

## Mappe

Le mappe in `mappe/` si aprono con un browser. La mappa degli orizzonti legge i dati dai due file
`dati_shic_orizzonti_1.js` e `_2.js`, che devono stare nella stessa cartella dell'HTML.
Per la base cartografica serve la connessione a internet.
