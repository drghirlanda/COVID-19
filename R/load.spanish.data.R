load.spanish.data <- function() {
    sp.names <- c(
        casos="Confirmed Cases",
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
        dt$Day <- as.Date( dt$Day )
        sp <- rbind( sp, dt )
    }
    
    ## normalize region names
    sp[ grep("Andaluc",Subregion), Subregion := "Andalucia" ]
    sp[ grep("Arag",Subregion), Subregion := "Aragon" ]
    sp[ grep("Castilla",Subregion), Subregion := "Castilla y Leon" ]
    sp[ grep("Catalu",Subregion), Subregion := "Cataluna" ]
    sp[ grep("Vasco",Subregion), Subregion := "Pais Vasco" ]
    
    sp
}
