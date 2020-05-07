Einleitung
==========

Daten
=====

Sie zur grundsätzlichen Beschreibung des Datensatzes siehe die
[Einführung in die Datenvisualisierung durch
Chernoff-Faces](https://maltehueckstaedt.github.io/Chernoff_Face/_pages/Chernoff/)
abschnitt
[Daten](https://maltehueckstaedt.github.io/Chernoff_Face/_pages/Chernoff/#daten)

Datenaufbereitung
=================

In einem ersten Schritt wird für die Datenaufbereitung mittels der
*tidyverse*-Pakete (Wickham 2017) der oben beschriebene Datensatz in die
Arbeitsumgebung geladen. Dies erfolgt hier mit der Funktion
`read_excel()` des Paketes *readxl* (Wickham and Bryan 2019).

``` r
# Lade Datensatz
library(readxl)
library(tidyverse)
df <- read_excel("/Users/maltehuckstadt/Google Drive/GitHub/Chernoff_Face/_pages/MCA_files/df_artiprice_061219.2.xlsx")
colnames(df)
```

    ##  [1] "Name"                             "...2"                            
    ##  [3] "...3"                             "...4"                            
    ##  [5] "...5"                             "...6"                            
    ##  [7] "Kunstfor_Biennalegespraeche"      "Kunstfor_Gespraeche"             
    ##  [9] "Kunstfor_Artikel_Autor"           "Kunstfor_Monograf_Portraits"     
    ## [11] "Kunstfor_Wichtige_Erwaehn"        "Kunstfor_Sonst_Nenn"             
    ## [13] "Kunstfor_Gespraech_Kuenstlern"    "Kunstfor_Kernpersonen_Gespraeche"
    ## [15] "Kunstfor_Ausstellungsrezensionen" "Geburtsort"                      
    ## [17] "Quelle_Geburtsort"                "Ost_West"                        
    ## [19] "Media"                            "Gender"                          
    ## [21] "Nationalitaet"                    "Alter"                           
    ## [23] "Lives_Works"                      "Most_Exibit_in_1"                
    ## [25] "Most_Exibit_in_2"                 "Most_Exibit_in_3"                
    ## [27] "Kunsthochschule"                  "Fachbereich"                     
    ## [29] "Death"                            "Galerie"                         
    ## [31] "Most_Exebit_Gal_Rank_Global"      "Most_Exebit_Gal_Rank_Natio"      
    ## [33] "Most_With_Global_Rank_1"          "Most_With_Global_Rank_2"         
    ## [35] "Most_With_Global_Rank_3"          "Rank_Global"                     
    ## [37] "Rank_Germa"                       "Rank_Portugal"                   
    ## [39] "Rank_China"                       "Rank_Czechia"                    
    ## [41] "Rank_Spain"                       "Rank_USA"                        
    ## [43] "Rank_South_Africa"                "Rank_Belgium"                    
    ## [45] "Rank_France"                      "Rank_Netherlands"                
    ## [47] "Rank_Austria"                     "Rank_Turk"                       
    ## [49] "Rank_United Kingdom"              "Rank_Ita"                        
    ## [51] "Rank_Romania"                     "Rank_Sweden"                     
    ## [53] "Rank_Norw"                        "Rank_Finland"                    
    ## [55] "Rank_Switzerland"                 "Rank_New Zealand"                
    ## [57] "Rank_Mexico"                      "Rank_Japan"                      
    ## [59] "Rank_Australia"                   "Rank_Israel"                     
    ## [61] "Rank_Poland"                      "Rank_Canada"                     
    ## [63] "Rank_Korea (Republic of)"         "Rank_Hungary"                    
    ## [65] "Rank_Russian_Federation"          "Rank_Denmark"                    
    ## [67] "Rank_Iran"                        "Studienfach"                     
    ## [69] "Studienort"                       "Studium_bei"                     
    ## [71] "Artprice"                         "SOLO EXHIBITIONS"                
    ## [73] "GROUP EXHIBITIONS"                "BIENNALS"                        
    ## [75] "ART FAIRS"                        "DEALER DIRECTORY"                
    ## [77] "MUSEUM COLLECTIONS"

``` r
df <- df %>% mutate(Kunstfor_Monograf_Portraits = as.numeric(Kunstfor_Monograf_Portraits)) %>%
  mutate(Kunstfor_Monograf_Portraits = case_when(Kunstfor_Monograf_Portraits ==0 ~ 'keine mono. Portrait',
                                                      Kunstfor_Monograf_Portraits >0 ~ '1 oder mehr mono. Portrait'))
```

Im Rahmen der Datenerhebung der Rankingpositionen war für so gut wie
alle Professor\_innen eine globale Rankingposition einfach zu ermitteln.
Komplizierter ist die Erhebnung der nationalen Rankingposition, die sich
Anfang 2019 bei Artfacts noch an der Geburtsstätte der jeweiligen
Künstler\_in orientierte. Um den Informationsgehalt der Daten nicht
schon im Rahmen der Erhebung zu reduzieren, wurde für jede auftretende
Nationalität ein neuer Spaltenvektor erzeugt, der die jeweilige,
nationale Rankingposition enthält:

``` r
head(df[,14:36])
```

    ## # A tibble: 6 x 23
    ##   Kunstfor_Kernpe… Kunstfor_Ausste… Geburtsort Quelle_Geburtso… Ost_West
    ##   <chr>            <chr>            <chr>      <chr>            <chr>   
    ## 1 0                0                Remscheid… <NA>             west    
    ## 2 0                0                München, … <NA>             west    
    ## 3 0                0                München, … <NA>             west    
    ## 4 0                0                Göttingen… <NA>             west    
    ## 5 0                0                Fritzlar,… <NA>             west    
    ## 6 0                0                <NA>       <NA>             <NA>    
    ## # … with 18 more variables: Media <chr>, Gender <chr>,
    ## #   Nationalitaet <chr>, Alter <chr>, Lives_Works <chr>,
    ## #   Most_Exibit_in_1 <chr>, Most_Exibit_in_2 <chr>,
    ## #   Most_Exibit_in_3 <chr>, Kunsthochschule <chr>, Fachbereich <chr>,
    ## #   Death <lgl>, Galerie <chr>, Most_Exebit_Gal_Rank_Global <chr>,
    ## #   Most_Exebit_Gal_Rank_Natio <chr>, Most_With_Global_Rank_1 <chr>,
    ## #   Most_With_Global_Rank_2 <chr>, Most_With_Global_Rank_3 <chr>,
    ## #   Rank_Global <chr>

Für die folgenden Analysen müssen diese Spaltenvektoren zu einem
einzigen Vektor zusammengefügt werden, der den Namen `Rank_Natio` tragen
soll:

``` r
#Setzt NA auf "" und füge Spalten zusammen
library(tidyverse)
df <- df %>% replace(., is.na(.), "") %>% unite("Rank_Natio", c(14:36))

# Entferne Unterstrich, die durch die unite_Funktion verusacht wurden.
df$Rank_Natio <- str_replace_all(df$Rank_Natio, "(_+)", "") #verwandele "_" (oder mehr) in ""

#ersetze leere String-Zellen durch NA Data-Frame-weit
df <- mutate_all(df, funs(na_if(.,"")))
```

Literatur
=========

Wickham, Hadley. 2017. *Tidyverse: Easily Install and Load the
’Tidyverse’*. <https://CRAN.R-project.org/package=tidyverse>.

Wickham, Hadley, and Jennifer Bryan. 2019. *Readxl: Read Excel Files*.
<https://CRAN.R-project.org/package=readxl>.
