sat <- function(t,x) { 
    n <- x*t + (1-x*t)*2^t
    n[ x*t>=1 ] <- max(n)
    n
}

png( "saturation.png", width=400, height=400 )
par( las=1 )
plot(
    sat(1:20,0),
    type="l",
    log="y",
    xlab="t / Td",
    ylab="",
    main="Casi in terapia intensiva (modello)" )
lines( sat(1:20,0.05), col=2 )
lines( sat(1:20,0.1), col=3 )
legend(
    "topleft",
    legend=c(
        "Nessuna saturazione",
        "Saturazione = 5% per Td",
        "Saturazione = 10% per Td"
    ),
    col=1:3,
    lty=1,
    bty="n"
)
dev.off()
