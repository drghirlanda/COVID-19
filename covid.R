data(state)

load.jhu.csse.data <- function( what ) {
    ## read raw data file
    file <- paste0(
        "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-",
        what,
        ".csv"
    )
    dt <- fread( file )

    ## shorter names
    setnames(
        dt,
        c("Province/State","Country/Region"),
        c("Subregion","Region")
    )

    ## there is extra whitespace in some cases
    dt[, Subregion := trimws( Subregion ) ]

    ## transform to long form
    dt <- melt( dt, id.vars=c("Subregion","Region","Lat","Long") )
    setnames( dt, c("variable","value"), c("Day","Count") )

    ## make sure Day is a date
    dt$Day <- as.Date( dt$Day, format="%m/%d/%y" )

    ## remove empty lines
    dt <- dt[ Count>0 ]
    
    ## The US data set has some inconsistencies and includes entries
    ## for both parts of states (counties, parishes) and for whole
    ## states. The former usually start earlier. Here we aggregate the
    ## data.

    us <- NULL
    for( s in state.abb ) {
        ## aggregate local data in this state 
        dt.s1 <- dt[ Region=="US" ][
            grep( paste0(", ", s, "$"), Subregion ),
            .(
                Region,
                rep( state.name[ state.abb==s ], .N ),
                mean( Lat ),
                mean( Long ),
                sum( Count, na.rm=TRUE )
            ),
            by=Day
        ]
        setnames(
            dt.s1,
            c("V2","V3","V4","V5"),
            c("Subregion","Lat","Long","Count")
        )
        ## get state data
        dt.s2 <- dt[ Region=="US" & Subregion==state.name[ state.abb==s ] ]
        ## merge and average
        dt.s <- rbind( dt.s1, dt.s2 )
        dt.s <- dt.s[,
                     .(
                         mean(Lat),
                         mean(Long),
                         mean(Count, na.rm=TRUE)
                     ),
                     by=.(Day,Region,Subregion)
                     ]
        setnames(
            dt.s,
            c("V1","V2","V3"),
            c("Lat","Long","Count")
        )
        ## delete old data
        us <- rbind( us, dt.s )
    }
    ## remove old US data and add new
    dt <- dt[ Region != "US" ]
    dt <- rbind( dt, us )
    
    ## an empty subregion means the total
    dt[ Subregion=="", Subregion := "All" ]

    ## add All subregion if not present
    for( r in unique(dt$Region) ) {
        if( ! "All" %in% dt[Region==r,Subregion] ) {
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

    dt$What <- what

    dt
}    

load.covid.data <- function() {
    covid <- fread("covid.csv")
    covid$Day <- as.Date( covid$Day, format="%Y-%m-%d" )
    covid
}

covid.sorted.regions <- function( covid ) {
    covid[
        What=="Fatalities",
        max(Count,na.rm=TRUE),
        by=Region
    ][
        order(-V1),
        Region
    ]
}

covid.sorted.subregions <- function( covid, region ) {
    covid[
        What=="Fatalities" &
        Region == region,
        max(Count,na.rm=TRUE),
        by=Subregion
    ][
        order(-V1),
        Subregion
    ]
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

exp.plot <- function( fit.out, add=FALSE, npred=0, col=1, model=TRUE, data=TRUE ) {
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
    if( data==TRUE ) {
        fit.out$dt[, points( Count ~ Day, pch=16, col=col ) ]
    }
    if( model==TRUE & sum( ! is.na( pred.count ) ) > 0 ) {
        lines( pred.days, pred.count, col=col )
        lines( pred.days2, pred.count2, col=col, lty=3 )
    }
    data.table(
        Day=as.Date( pred.days ),
        Count=pred.count
    )
}

