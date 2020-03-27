load.italian.data <- function() {
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
        c("Day","In Hospital","In ICU","Fatalities","Confirmed Cases")
    )

    ## retain day, drop time:
    italy[, Day := as.Date( Day, format="%Y-%m-%d" ) ]

    italy <- melt( italy, id.vars=c("Subregion","Region","Day") )
    setnames( italy, c("variable","value"), c("What","Count") )

    italy
}
