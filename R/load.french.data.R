load.french.data <- function() {
    fr.raw <- fread("https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv")
    fr <- rbind(
        fr.raw[ maille_code=="FRA" ],
        fr.raw[ grep("REG-",maille_code) ]
    )

    setnames( fr, c("date","maille_nom"), c("Day","Subregion") )
    fr[ Subregion=="France", Subregion := "All" ]
    fr[, Region := "France" ]

    ## fix some inconsistencies in names
    fr[ grep("Auvergne",Subregion), Subregion := "Auvergne-Rhone-Alpes" ]
    fr[ grep("Grand",Subregion), Subregion := "Grand Est" ]
    fr[ grep("Bourgogne",Subregion), Subregion := "Bourgogne-Franche-Comte" ]
    fr[ grep("Provence",Subregion), Subregion := "Provence-Alpes-Cote d'Azur" ]
    fr[ grep("Centre",Subregion), Subregion := "Centre-Val de Loire" ]
    fr[ grep("La R",Subregion), Subregion := "La Reunion" ]

    fr <- fr[, .(Subregion,Region,Day,cas_confirmes,deces) ]

    fr.c <- fr[, .(Subregion,Region,Day,cas_confirmes) ]
    setnames( fr.c, "cas_confirmes", "Count" )
    fr.c$What <- "Confirmed Cases"
    fr.d <- fr[, .(Subregion,Region,Day,deces) ]
    setnames( fr.d, "deces", "Count" )
    fr.d$What <- "Fatalities"
    
    fr <- rbind( fr.c, fr.d )
    
    fr <- fr[ complete.cases(fr) ]

    ## get rid of some inconsistencies and duplications
    fr <- fr[, mean(Count), by=.(Subregion,Region,Day,What) ]
    setnames( fr, "V1", "Count" )

    fr$Day <- as.Date( fr$Day )

    fr
}
