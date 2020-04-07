load.italian.data <- function() {
    ## national-level data
    italy <- fread("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
    italy <- italy[, .(data,ricoverati_con_sintomi,terapia_intensiva,deceduti,totale_casi) ]
    italy$Reg1 <- "Italy"
    italy$Reg2 <- "All"

    ## region-level data
    italy.reg <- fread("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
    italy.reg <- italy.reg[, .(denominazione_regione,data,ricoverati_con_sintomi,terapia_intensiva,deceduti,totale_casi) ]
    italy.reg$Reg1 <- "Italy"
    setnames( italy.reg, "denominazione_regione", "Reg2" )

    italy <- rbind( italy, italy.reg )
    setnames(
        italy,
        c("data","ricoverati_con_sintomi","terapia_intensiva","deceduti","totale_casi"),
        c("Day","In Hospital","In ICU","Fatalities","Confirmed Cases")
    )

    ## retain day, drop time:
    italy[, Day := as.Date( Day, format="%Y-%m-%d" ) ]

    italy <- melt( italy, id.vars=c("Reg2","Reg1","Day") )
    setnames( italy, c("variable","value"), c("What","Count") )

    no.lombardy <- italy[ ! Reg2 %in% c("All","Lombardia"), sum(Count), by=.(Day,Reg1,What) ]
    setnames( no.lombardy, "V1", "Count" )
    no.lombardy$Reg2 <- "Without Lombardia"
    
    italy <- rbind( italy, no.lombardy )
    
    italy$Reg3 <- "All"
    italy
}
