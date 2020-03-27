data(state)

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

exp.plot <- function( fit.out, add=FALSE, npred=0, col=1, model=TRUE, data=TRUE, confint=FALSE ) {
    first.day <- min( fit.out$dt$Day )
    last.day <- max( fit.out$dt$Day )
    pred.days <- as.Date( first.day : (last.day + npred) )
    pred.days2 <- as.Date( (first.day-10) : (last.day + 10) )
    if( ! is.null( fit.out$fit ) ) {
        pred.count <- predict(
            fit.out$fit,
            newdata=data.frame(Day=pred.days),
            interval="confidence"
        )
        pred.count <- 2 ^ pred.count
        pred.count2 <- predict(
            fit.out$fit,
            newdata=data.frame(Day=pred.days2),
            interval="confidence"
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
        lines( pred.days, pred.count[,1], col=col )
        lines( pred.days2, pred.count2[,1], col=col, lty=3 )
        if( confint ) {
            polygon(
                c(pred.days,rev(pred.days)),
                c(pred.count[,3],rev(pred.count[,2])),
                col=adjustcolor( col, alpha=.15 ),
                border=NA
            )
            polygon(
                c(pred.days2,rev(pred.days2)),
                c(pred.count2[,3],rev(pred.count2[,2])),
                col=adjustcolor( col, alpha=.1 ),
                border=NA
            )
        }
    }
    data.table(
        Day=as.Date( pred.days ),
        Count=pred.count[,1]
    )
}

