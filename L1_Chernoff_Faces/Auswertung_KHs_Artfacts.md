# Chernoff Faces

Die von Herman Chernoff (\* 1. Juli 1923) entwickelten *Chernoff-Faces*
(1973) sind ein verfahren der multivariaten Datenvisualisierung. Im
Rahmen er Datenvisualisierung mittels Chernoff Faces wird die
Physiognomie eines cartoonartig simplifizierten, menschlichen Gesichts
(wie z.B. Größe der Ohren, Form des Mundes, Neigung der Augenbrauen,
Größe der Nase usw.) den Merkmalsausprägungen eines Merkmalsträgers
entsprechend geformt. In der Folge erhält jeder Zeilenvektor eines
Datensatzes ein seiner Kombination von Spaltenvektor-Werten entsprechend
strukturiertes Chernoff-Face. Mittels der je nach Werten auf den
Spaltenvekotoren ggf. variierenden Chernoff-Gesichter können nun die
Merkmalsprofile der Zeilenvektoren untereinander verglichen werden. Eine
markante Stärke der Visualisierungsmethode der Chernoff Faces ist es,
dass sie die menschliche Fähigkeit nutzt, auch kleine Divergenzen oder
Konvergenzen in der Physiognomie menschlicher (bzw. menschenähnlicher)
Gesichter zu registrieren. Da die Physiognomie der Chernoff Faces
gewissermaßen *kurzgeschlossen* ist, mit den Merkmalsausprägungen der
jeweiligen Merkmalsträger, erlaubt die Methode der Chernoff-Faces
Ähnlich- bzw. Unähnlichkeitsstrukturen verschiedener Merkmalsprofile
von Zeilenvektoren leicht und intuitiv zu explorieren.

Problematisch wird die Anwendung der Chernoff Faces bei Datensets (vgl.
Livingstone 2009) mit sehr vielen Zeilenvektoren: Selbstverständlich
sind zwanzig Gesichter leichter zu unterscheiden als zweihundert.

# Daten

Der im folgenden zur Illustration verwendete Datensatz weißt für
sämtliche Professor\_innen bundesdeutscher, staatlicher
Kunsthochschulen im Fachbereich bildende Kunst (und angrenzender Fächer)
u.a. folgende Variablen auf:

  - nationale und internationale Rankingposition des
    Künstler\_innen-Rankings ([Artfacts](https://artfacts.net/))
  - Alter
  - Geschlecht

Es können nun in diesen, vorgenannten Dimensionen statistische Profile
für deutsche, alle staatlichen Kunsthochschulen erzeugt werden, die
wiederum mittels der Chernoff Faces visualisiert und exploriert werden
können. Hierzu sollen folgende, einfache Kennzahlen pro Kunsthochschule
berechnet werden:

1.  Mittelwert des globalen Rankings aller Professor\_innen einer
    Kunsthochschule
2.  Mittelwert des nationalen Rankings aller Professor\_innen einer
    Kunsthochschule
3.  Anzahl der Professor\_innen pro Kunsthochschule ingesamt
4.  Anzahl weiblicher Professor\_innen pro Kunsthochschule
5.  Anzahl männlicher Professor\_innen pro Kunsthochschule

# Datenaufbereitung

In einem ersten Schritt wird für die Datenaufbereitung mittels der
*tidyverse*-Pakete (Wickham 2017) der oben beschriebene Datensatz in die
Arbeitsumgebung geladen. Dies erfolgt hier mit der Funktion
`read_excel()` des Paketes *readxl* (Wickham and Bryan 2019).

``` r
# Lade Datensatz
library(readxl)
df <- read_excel("Fine_Artfacts_Kunsthochschulen.xlsx")
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
    ##   Rank_Germany Rank_USA Rank_France Rank_Netherlands Rank_Austria
    ##          <dbl>    <dbl>       <dbl>            <dbl>        <dbl>
    ## 1            7       NA          NA               NA           NA
    ## 2           24       NA          NA               NA           NA
    ## 3           33       NA          NA               NA           NA
    ## 4           34       NA          NA               NA           NA
    ## 5           40       NA          NA               NA           NA
    ## 6           NA       NA          NA               NA           NA
    ## # … with 18 more variables: Rank_Turkey <dbl>, `Rank_United
    ## #   Kingdom` <dbl>, Rank_Italy <dbl>, Rank_Romania <dbl>,
    ## #   Rank_Sweden <dbl>, `Rank_South Africa` <dbl>, Rank_Norway <dbl>,
    ## #   Rank_Finland <dbl>, Rank_Switzerland <dbl>, `Rank_New Zealand` <dbl>,
    ## #   Rank_Mexico <dbl>, Rank_Japan <dbl>, Rank_Australia <dbl>,
    ## #   Rank_Israel <dbl>, Rank_Poland <dbl>, Rank_Canada <dbl>, `Rank_Korea
    ## #   (Republic of)` <dbl>, Rank_Iran <dbl>

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

Des Weiteren wurden die Ranking-Variablen als String-Variablen in die
Arbeitsumgebung geladen, der einfachheit halber werden diese einzeln in
numerische Variablen umgewandelt:

``` r
#colnames(df)
df$Rank_Global <- as.numeric(df$Rank_Global)
df$Rank_Natio <- as.numeric(df$Rank_Natio)
df$Kunsthochschule <- as.factor(df$Kunsthochschule)
```

Im folgenden werden nun aggregierte Variablen auf Kunsthochschulebene
erzeugt. Dieses sind, wie oben bereits beschrieben:

1.  Mittelwert des globalen Rankings aller Professor\_innen einer
    Kunsthochschule
2.  Mittelwert des nationalen Rankings aller Professor\_innen einer
    Kunsthochschule
3.  Anzahl der Professor\_innen pro Kunsthochschule ingesamt
4.  Anzahl weiblicher Professor\_innen pro Kunsthochschule
5.  Anzahl männlicher Professor\_innen pro Kunsthochschule

<!-- end list -->

``` r
# Erzeuge aggregierte Variablen auf KH-Ebene

# Ranking Durchschnittwerte pro Kunsthochschule
Rank <- df %>% group_by(Kunsthochschule) %>% mutate(Mean_Nat_Rank= mean(Rank_Natio,na.rm = T)) %>% mutate(Mean_Global_Rank = mean(Rank_Global, na.rm = T)) %>%
  summarize(Sum_Nat = round(mean(Mean_Nat_Rank),2), Sum_Glob = round(mean(Mean_Global_Rank),2))

#Anzahl Professoren pro Kunsthochschule
Profs <- df %>% group_by(Kunsthochschule) %>% summarise (n_Profs = n())

# Anzahl geschlecht pro Kunsthochschule
Gender <- df %>% group_by(Kunsthochschule) %>%
  summarise(n_Frau = sum(Gender == "F"), n_Mann = sum(Gender == "M")) 

# Mean_Alter pro Kunsthochschule
df$Alter <- as.numeric(df$Alter)
df$Alter <- 2019-df$Alter
 
Alter <- df %>% group_by(Kunsthochschule) %>% mutate(Mean_Alter= mean(Alter,na.rm = T))  %>%  
  summarize(Mean_Alter_Sum = round(mean(Mean_Alter),2))   

# Datenzusammenführung:
df1 <- merge(Rank,Profs, by="Kunsthochschule")
df2 <- merge(df1,Gender, by="Kunsthochschule")
ArtFac <- merge(df2,Alter, by="Kunsthochschule")

# entferne alle Dataframes außer ArtFac + df
rm(list=setdiff(ls(), c("ArtFac", "df")))
```

Schließlich werden alle Zeilen aus dem Dataframe gefiltert, die einen
oder mehrere Missings auf den Spaltenvektoren aufweisen. Darüberhinaus
werden, um den anschließend zu erzeugenden Chernoff-Faces-Plot
übersichtlich zu gestallten, die Namen der Kunsthochschulen durch
Acronyme ersetzt und als `rownames` spezifiziert.

``` r
# colnames(ArtFac)
# filtere NA-Zeile raus
ArtFac <- ArtFac %>%  filter(!is.na(Sum_Nat))

# Columns in Rownames
ArtFac <- ArtFac %>% remove_rownames %>% column_to_rownames(var="Kunsthochschule")

#rownames(ArtFac)
# ändere Namen der KHs um Faces Plot Übersichtlicher zu gestallten
rownames(ArtFac)[1]<-"AdBK M"
rownames(ArtFac)[2]<-"BU WE"
rownames(ArtFac)[3]<-"KH HAL"
rownames(ArtFac)[4]<-"HBK BS"
rownames(ArtFac)[5]<-"HfBK DD"
rownames(ArtFac)[6]<-"HfBK HH"
rownames(ArtFac)[7]<-"HFK HB"
rownames(ArtFac)[8]<-"HdBK SB"
rownames(ArtFac)[9]<-"HfG OF"
rownames(ArtFac)[10]<-"KA D"

rownames(ArtFac)[11]<-"KA MS" 
rownames(ArtFac)[12]<-"KHB B" 
rownames(ArtFac)[13]<-"KfM K" 
rownames(ArtFac)[14]<-"KH KS" 
rownames(ArtFac)[15]<-"KH KI" 
rownames(ArtFac)[16]<-"KH L" 
rownames(ArtFac)[17]<-"KH MZ" 
rownames(ArtFac)[18]<-"KH N" 
rownames(ArtFac)[19]<-"AdK KA" 
rownames(ArtFac)[20]<-"AdK S" 
rownames(ArtFac)[21]<-"HfBK FaM" 
rownames(ArtFac)[22]<-"UdK Berlin" 
```

``` r
library(aplpack)
faces(ArtFac[1:22,],face.type=1)
```

![](Auswertung_KHs_Artfacts_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

    ## effect of variables:
    ##  modified item       Var             
    ##  "height of face   " "Sum_Nat"       
    ##  "width of face    " "Sum_Glob"      
    ##  "structure of face" "n_Profs"       
    ##  "height of mouth  " "n_Frau"        
    ##  "width of mouth   " "n_Mann"        
    ##  "smiling          " "Mean_Alter_Sum"
    ##  "height of eyes   " "Sum_Nat"       
    ##  "width of eyes    " "Sum_Glob"      
    ##  "height of hair   " "n_Profs"       
    ##  "width of hair   "  "n_Frau"        
    ##  "style of hair   "  "n_Mann"        
    ##  "height of nose  "  "Mean_Alter_Sum"
    ##  "width of nose   "  "Sum_Nat"       
    ##  "width of ear    "  "Sum_Glob"      
    ##  "height of ear   "  "n_Profs"

``` r
library(stringr)
colnames(df)
```

    ##  [1] "Name"             "Media"            "Gender"          
    ##  [4] "Nationalitaet"    "Alter"            "Lives_Works"     
    ##  [7] "Most_Exibit_in_1" "Most_Exibit_in_2" "Most_Exibit_in_3"
    ## [10] "Kunsthochschule"  "Fachbereich"      "Death"           
    ## [13] "Rank_Global"      "Rank_Natio"       "Studienfach"     
    ## [16] "Studienort"       "Studium_bei"

``` r
df$KH_Name_Short <- df$Kunsthochschule
table(df$KH_Name_Short)
```

    ## 
    ##                                      Akademie der Bildenden Künste München 
    ##                                                                         17 
    ##                                                 Bauhaus-Universität Weimar 
    ##                                                                          4 
    ##                                  Burg Giebichenstein Kunsthochschule Halle 
    ##                                                                         14 
    ##                                                           HBK Braunschweig 
    ##                                                                         13 
    ##                                                               HfBK Dresden 
    ##                                                                         12 
    ##                                                               HfbK Hamburg 
    ##                                                                         16 
    ##                                                                 HFK Bremen 
    ##                                                                          9 
    ##                                       Hochschule der Bildenden Künste Saar 
    ##                                                                          5 
    ##                                        Hochschule für Gestaltung Offenbach 
    ##                                                                          5 
    ##                                                   Kunstakademie Düsseldorf 
    ##                                                                         19 
    ##                                                      Kunstakademie Münster 
    ##                                                                         13 
    ##                                           Kunsthochschule Berlin Weißensee 
    ##                                                                          6 
    ##                                            Kunsthochschule für Medien Köln 
    ##                                                                          8 
    ##                                                     Kunsthochschule Kassel 
    ##                                                                          6 
    ##                                                       Kunsthochschule Kiel 
    ##                                                                          6 
    ##                                                    Kunsthochschule Leipzig 
    ##                                                                         18 
    ##                                                      Kunsthochschule Mainz 
    ##                                                                         11 
    ##                                                   Kunsthochschule Nürnberg 
    ##                                                                         10 
    ##                         Staatliche Akademie der Bildenden Künste Karlsruhe 
    ##                                                                         14 
    ##                         Staatliche Akademie der Bildenden Künste Stuttgart 
    ##                                                                         16 
    ## Staatliche Hochschule für Bildende Künste (Städelschule) Frankfurt am Main 
    ##                                                                          8 
    ##                                                                 UdK Berlin 
    ##                                                                         22

``` r
class(df$KH_Name_Short)
```

    ## [1] "factor"

``` r
#df$KH_Name_Short <- as.character(df$KH_Name_Short)

df$KH_Name_Short <- df$KH_Name_Short %>%
        str_replace("Akademie der Bildenden Künste München","AdBK M") %>%    
        str_replace("Bauhaus-Universität Weimar","BU WE") %>%
        str_replace("Burg Giebichenstein Kunsthochschule Halle","KH HAL") %>%
        str_replace("HBK Braunschweig","HBK BS") %>%
        str_replace("HfBK Dresden","HfBK DD") %>%  

        str_replace("HfbK Hamburg","HfBK HH") %>%
        str_replace("HFK Bremen","HFK HB") %>%
        str_replace("Hochschule der Bildenden Künste Saar", "HdBK SB") %>%
        str_replace("Hochschule für Gestaltung Offenbach", "HfG OF") %>%
        str_replace("Kunstakademie Düsseldorf","KA D") %>%
        
        str_replace("Kunstakademie Münster","KA MS") %>%
        str_replace("Kunsthochschule Berlin Weißensee","KHB B") %>%
        str_replace("Kunsthochschule für Medien Köln","KfM K") %>%
        str_replace("Kunsthochschule Kassel","KH KS") %>%
        str_replace("Kunsthochschule Kiel","KH KI") %>%
        
        str_replace(" \\(.*\\)", "")  %>% # einzelnd Klammern entfernen, sonst funktioniert str_replace bei  HfBK FaM nicht

        str_replace("Kunsthochschule Leipzig","KH L") %>%
        str_replace("Kunsthochschule Mainz","KH MZ") %>%
        str_replace("Kunsthochschule Nürnberg","KH N") %>%
        str_replace("Staatliche Akademie der Bildenden Künste Karlsruhe","AdK KA") %>%
        str_replace("Staatliche Akademie der Bildenden Künste Stuttgart","AdK S") %>% 
        str_replace("Staatliche Hochschule für Bildende Künste Frankfurt am Main","HfBK FaM") %>%
        str_replace("UdK Berlin","UdK B")

#Gender dichothomisieren:
df$Gender <- df$Gender %>%
        str_replace("M, F","F")  

 df_gg <- df %>% select(Gender,KH_Name_Short,Rank_Global,Rank_Natio) %>% na.omit()

# Plot mit facet_wrap, also pro HS eine Plottafel (etwas unübersichtlich!)
# ggplot(df_gg, aes(x=factor(Gender), y=Rank_Global, fill=factor(Gender)) ) + 
#   #geom_point(size=2, alpha=.6,aes(colour=factor(Gender)))+
#   geom_jitter(size=2, alpha=.6,aes(colour=factor(Gender))) +
#   geom_boxplot(alpha = 0.5, show.legend = FALSE) + 
#   facet_wrap(.~ KH_Name_Short)  

#Rank Global über Khs und Geschlecht
ggplot(df_gg, aes(x=factor(KH_Name_Short), y=Rank_Global, fill=factor(Gender)) ) + 
  #geom_point(size=2, alpha=.6,aes(colour=factor(Gender)))+
  geom_jitter(size=2, alpha=.6,aes(colour=factor(Gender))) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE)+
  theme(axis.text.x = element_text(angle=45, hjust=1))
```

![](Auswertung_KHs_Artfacts_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#Rank National über Khs und Geschlecht
ggplot(df_gg, aes(x=factor(KH_Name_Short), y=Rank_Natio, fill=factor(Gender)) ) + 
  #geom_point(size=2, alpha=.6,aes(colour=factor(Gender)))+
  geom_jitter(size=2, alpha=.6,aes(colour=factor(Gender))) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE)+
  theme(axis.text.x = element_text(angle=45, hjust=1))
```

![](Auswertung_KHs_Artfacts_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

# Literatur

<div id="refs" class="references">

<div id="ref-chernoff1973use">

Chernoff, Herman. 1973. “The Use of Faces to Represent Points in
K-Dimensional Space Graphically.” *Journal of the American Statistical
Association* 68 (342): 361–68.

</div>

<div id="ref-livingstone2009practical">

Livingstone, David. 2009. *A Practical Guide to Scientific Data
Analysis*. Vol. 341. Wiley Online Library.

</div>

<div id="ref-tidyverse2017">

Wickham, Hadley. 2017. *Tidyverse: Easily Install and Load the
’Tidyverse’*. <https://CRAN.R-project.org/package=tidyverse>.

</div>

<div id="ref-readxl2019">

Wickham, Hadley, and Jennifer Bryan. 2019. *Readxl: Read Excel Files*.
<https://CRAN.R-project.org/package=readxl>.

</div>

</div>
