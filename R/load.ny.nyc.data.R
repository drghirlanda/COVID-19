load.ny.nyc.data <- function() {

    ny <- fread("https://docs.google.com/spreadsheets/d/1ixC6SxKs_4wYBPpsqwVAaihFEHj8SAN-3hIC2dluWrg/export?format=csv")
    ny <- melt( ny, id.vars="Location" )
    setnames( ny, c("variable","value"), c("Day","Count") )
    ny$Day <- as.Date( ny$Day, format="%F" )
    ny$Count <- as.numeric( sub( ",", "", ny$Count, fixed=TRUE ) )
    ny <- ny[, mean(Count), by=.(Location,Day) ]
    setnames( ny, "V1", "Count" )

    ny$What <- "Confirmed Cases"
    ny <- ny[ complete.cases(ny) ]
    ny$Region <- "US"
    
    nyc <- ny[ Location == "New York City" ]
    setnames( nyc, "Location", "Subregion" )
    nyc$Region <- "US"
    nyc <- rbind(
        nyc,
        data.table(
            Subregion="New York City",
            Region="US",
            Count=192,
            Day=as.Date("2020-03-24"),
            What="Fatalities"
        )
    )

    nyc$Count <- as.character( nyc$Count )

    ## ny totals
    ny <- ny [ ! grep( "Total", Location), sum(Count), by=.(Day,What) ]
    setnames( ny, "V1", "Count" )
    ny$Region <- "US"
    ny$Subregion <- "New York"

    rbind( ny, nyc )
}
