load.covid <- function( file ) {
    ## read raw data file
    file <- paste0(
        "csse_covid_19_data/",
        "csse_covid_19_time_series/",
        "time_series_19-covid-",
        file,
        ".csv"
    )
    dt <- fread( file )

    ## shorter names
    setnames(
        dt,
        c("Province/State","Country/Region"),
        c("Subregion","Region")
    )

    ## transform to long form
    dt <- melt( dt, id.vars=c("Subregion","Region","Lat","Long") )
    setnames( dt, c("variable","value"), c("Day","Count") )

    ## make sure Day is a date
    dt$Day <- as.Date( dt$Day, format="%m/%d/%y" )

    ## create New York City subregion
    nyc.counties <- c(
        "New York County, NY",
        "Kings County, NY",
        "Queens County, NY",
        "Richmond County, NY",
        "Bronx County, NY"
    )
    nyc.dt <- dt[
        Subregion %in% nyc.counties,
        .(
            "New York City",
            "US",
            mean(Lat),
            mean(Long),
            sum(Count,na.rm=TRUE)
        ),
        by=Day
    ]

    setnames(
        nyc.dt,
        c("V1","V2","V3","V4","V5"),
        c("Subregion","Region","Lat","Long","Count")
    )
    dt <- rbind( dt, nyc.dt )
    
    ## remove county-level data
    dt <- dt[ ! grep("County", Subregion) ]
    dt <- dt[ ! grep("Parish", Subregion) ]

    dt[ Subregion=="", Subregion := "All" ]

    ## add All subregion if not present
    for( r in unique(dt$Region) ) {
        if( sum( dt[Region==r,Subregion=="All"] == 0 ) ) {
            all.dt <- dt[
                Region==r,
                .(
                    "All",
                    r,
                    mean(Lat),
                    mean(Long),
                    sum(Count,na.rm=TRUE)
                ),
                by=Day
            ]
            setnames(
                all.dt,
                c("V1","r","V3","V4","V5"),
                c("Subregion","Region","Lat","Long","Count")
            )
            dt <- rbind( dt, all.dt )
        }
    }

    dt[, RegionTotal := sum(Count,na.rm=TRUE), by=Region ]
    dt[, SubregionTotal := sum(Count,na.rm=TRUE), by=.(Region,Subregion) ]
    
    dt
}    

get.totals <- function( dt ) {
    dt.tot <- dt[, sum(Count, na.rm=TRUE), by=Day ]
    setnames( dt.tot, "V1", "Count" )
    dt.tot
}

exp.fit <- function( dt, min.count=1 ) {
    t0 <- dt[ Count>=min.count, min(Day) ]
    dt <- dt[ Day>=t0 ]
    dt[, Log2Count := log(Count, base=2) ]
    dt.fit <- try( lm( Log2Count ~ Day, dt ) )
    if( class(dt.fit) == "try-error" ) {
        dt.fit <- NULL
    }
    list(
        dt = dt[ Day >= t0 ],
        fit = dt.fit
    )
}

exp.plot <- function( fit.out, add=FALSE, npred=0, col=1, model ) {
    first.day <- min( fit.out$dt$Day )
    last.day <- max( fit.out$dt$Day )
    pred.days <- as.Date( first.day : (last.day + npred) )
    pred.days2 <- as.Date( (first.day-10) : (last.day + 10) )
    if( ! is.null( fit.out$fit ) ) {
        pred.count <- predict(
            fit.out$fit,
            newdata=data.frame(Day=pred.days)
        )
        pred.count <- 2 ^ pred.count
        pred.count2 <- predict(
            fit.out$fit,
            newdata=data.frame(Day=pred.days2)
        )
        pred.count2 <- 2 ^ pred.count2
    } else {
        pred.count <- rep( NA, length(pred.days) )
        pred.count2 <- rep( NA, length(pred.days2) )
    }
    ymax <- max( c(fit.out$dt$Count, pred.count), na.rm=TRUE ) 
    par( las=1 )
    if( add==FALSE ) {
        plot(
            as.Date(NA), NA,
            xlim=as.Date( c(first.day,last.day+npred) ),
            ylim=c(1,ymax),
            xlab="Day",
            ylab="Count",
            log="y"
        )
    }
    fit.out$dt[, points( Count ~ Day, pch=16, col=col ) ]
    if( model & sum( ! is.na( pred.count ) ) > 0 ) {
        lines( pred.days, pred.count, col=col )
        lines( pred.days2, pred.count2, col=col, lty=3 )
    }
    data.table(
        Day=as.Date( pred.days ),
        Count=pred.count
    )
}

