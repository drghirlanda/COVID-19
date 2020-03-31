load.spanish.data <- function() {
    sp.names <- c(
        casos="Confirmed Cases",
        fallecidos="Fatalities",
        uci="ICU admissions"
    )
    
    sp <- NULL
    for( sp.what in names(sp.names) ) {
        dt <- fread(paste0("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_",sp.what,".csv"))
        dt <- melt( dt, id.vars="CCAA" )
        dt <- dt[ variable!="cod_ine" ]
        setnames(
            dt,
            c("CCAA","variable","value"),
            c("Reg2","Day","Count")
        )
        dt[ Reg2=="Total", Reg2 := "All" ]
        dt[ , Reg1 := "Spain" ]
        dt[ , What := sp.names[[sp.what]] ]
        dt$Day <- as.Date( dt$Day )
        sp <- rbind( sp, dt )
    }
    
    ## normalize region names
    sp[ grep("Andaluc",Reg2), Reg2 := "Andalucia" ]
    sp[ grep("Arag",Reg2), Reg2 := "Aragon" ]
    sp[ grep("Castilla",Reg2), Reg2 := "Castilla y Leon" ]
    sp[ grep("Catalu",Reg2), Reg2 := "Cataluna" ]
    sp[ grep("Vasco",Reg2), Reg2 := "Pais Vasco" ]

    sp$Reg3 <- "All"
    sp
}
