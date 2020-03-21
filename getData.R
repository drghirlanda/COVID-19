##

library(data.table)
library(zoo)
source("covid.R")

## jhu csse data
confirmed <- load.jhu.csse.data("Confirmed")
deaths <- load.jhu.csse.data("Deaths")
deaths$What <- "Fatalities"

covid <- rbind( confirmed, deaths )

## official Italian data
## national-level data
italy <- fread("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
italy <- italy[, .(data,ricoverati_con_sintomi,terapia_intensiva,deceduti,totale_casi) ]
italy$Region <- "Italy"
italy$Subregion <- "All"
## region-level data
italy.reg <- fread("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
italy.reg <- italy.reg[, .(denominazione_regione,data,ricoverati_con_sintomi,terapia_intensiva,deceduti,totale_casi) ]
italy.reg$Region <- "Italy"
setnames( italy.reg, "denominazione_regione", "Subregion" )

italy <- rbind( italy, italy.reg )
setnames(
    italy,
    c("data","ricoverati_con_sintomi","terapia_intensiva","deceduti","totale_casi"),
    c("Day","In Hospital","In ICU","Fatalities","Confirmed")
)

## retain day, drop time:
italy[, Day := as.Date( Day, format="%Y-%m-%d" ) ]

italy <- melt( italy, id.vars=c("Subregion","Region","Day") )
setnames( italy, c("variable","value"), c("What","Count") )

italy$Lat <- italy$Long <- NA

## the official Italian data are more accurate but do not cover the
## whole date range in the jhu data. we keep the jhu data when no
## official Italian data are available.
days <- unique( italy$Day )
covid <- covid[
    ! (Region=="Italy" &
       Day %in% days)
]
## merge Italian data
covid <- rbind( covid, italy )

## official French data
fr.raw <- fread("https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv")
 fr <- rbind(
    fr.raw[ maille_code=="FRA" ],
    fr.raw[ grep("REG-",maille_code) ]
)

setnames( fr, c("date","maille_nom"), c("Day","Subregion") )
fr[ Subregion=="France", Subregion := "All" ]
fr[, Region := "France" ]

## fix some inconsistencies in names
fr[ grep("Auvergne",Subregion), Subregion := "Auvergne-Rhóne-Alpes" ]
fr[ grep("Grand",Subregion), Subregion := "Grand Est" ]
fr[ grep("Bourgogne",Subregion), Subregion := "Bourgogne-Franche-Comté" ]
fr[ grep("Provence",Subregion), Subregion := "Provence-Alpes-Cóte d'Azur" ]
fr[ grep("Centre",Subregion), Subregion := "Centre-Val de Loire" ]
fr[ grep("La R",Subregion), Subregion := "La Réunion" ]

fr <- fr[, .(Subregion,Region,Day,cas_confirmes,deces) ]

fr.c <- fr[, .(Subregion,Region,Day,cas_confirmes) ]
setnames( fr.c, "cas_confirmes", "Count" )
fr.c$What <- "Confirmed"
fr.d <- fr[, .(Subregion,Region,Day,deces) ]
setnames( fr.d, "deces", "Count" )
fr.d$What <- "Fatalities"

fr <- rbind( fr.c, fr.d )

fr <- fr[ complete.cases(fr) ]

## get rid of some inconsistencies and duplications
fr <- fr[, mean(Count), by=.(Subregion,Region,Day,What) ]
setnames( fr, "V1", "Count" )

fr$Lat <- fr$Long <- NA
fr$Day <- as.Date( fr$Day )

covid <- covid[ ! Region=="France" ]
covid <- rbind( covid, fr )

## Spanish data
sp.names <- c(
    casos="Confirmed",
    fallecidos="Fatalities",
    uci="In ICU"
)
sp <- NULL
for( sp.what in names(sp.names) ) {
    dt <- fread(paste0("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_",sp.what,".csv"))
    dt <- melt( dt, id.vars="CCAA" )
    dt <- dt[ variable!="cod_ine" ]
    setnames(
        dt,
        c("CCAA","variable","value"),
        c("Subregion","Day","Count")
    )
    dt[ Subregion=="Total", Subregion := "All" ]
    dt[ , Region := "Spain" ]
    dt[ , What := sp.names[[sp.what]] ]
    sp <- rbind( sp, dt )
}
## normalize region names
sp[ grep("Andaluc",Subregion), Subregion := "Andalucía" ]
sp[ grep("Arag",Subregion), Subregion := "Aragón" ]
sp[ grep("Castilla",Subregion), Subregion := "Castilla y León" ]
sp[ grep("Catalu",Subregion), Subregion := "Cataluña" ]
sp[ grep("Vasco",Subregion), Subregion := "País Vasco" ]

sp$Lat <- sp$Long <- NA

days <- sp[ Subregion=="All", Day ]

covid <- covid[ ! (Region=="Spain" & Subregion=="All" & Day %in% days) ]
covid <- rbind( covid, sp )

fwrite( covid, "covid.csv" )
