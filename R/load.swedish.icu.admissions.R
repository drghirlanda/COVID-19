load.swedish.icu.admissions <- function() {

    ## this function opens a firefox browser and uses web automation
    ## provided by RSelenium to open the appropriate web page and
    ## click on the button that downloads the data. this complication
    ## is necessary because the page is generated in JS and there is
    ## no URL for the data...

    require(RSelenium)

    ## launch a virtual X server and set DISPLAY for firefox
    xpid <- system("eval 'echo $$; Xvfb :1'", wait=FALSE )
    Sys.setenv( DISPLAY=":1" )
    
    ## store the downloaded file here:
    download.dir <- "~/Downloads/"

    ## create a firefox profile that does not ask the user when
    ## downloading files
    fp <- makeFirefoxProfile(
        list(
            browser.download.dir=download.dir,
            browser.download.folderList="2",
            browser.download.manager.showWhenStarting="false",
            browser.helperApps.neverAsk.saveToDisk="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,application/vnd.ms-excel,application/octet-stream"
        )
    )

    ## create the selenium driver and firefox client
    message( "starting firefox" )
    d <- rsDriver(
        browser="firefox",
        verbose=FALSE,
        extraCapabilities=fp
    )

    ## navigate to the page
    message( "opening page" )
    d$client$navigate( "https://portal.icuregswe.org/siri/report/vtfstart-corona" )

    ## find the "Detaljer" element and click on it to switch to table
    ## view and create a download button
    message( "switching to table view" )
    detaljer <- d$client$findElement(
                             using="id",
                             value="tab-detail-table"
                         )
    detaljer$clickElement()

    ## find the download button and click on it
    message( "downloading file" )
    skapa <- d$client$findElement(
                          using="css selector",
                          value=".btn-sm"
                      )
    skapa$clickElement()

    ## at this point, the data file should have been downloaded in
    ## download.dir
    
    ## we are done with firefox and selenium
    message( "closing firefox" )
    d$client$close()
    d$server$stop()
    system(paste("kill",xpid))
    
    require(data.table)
    require(readxl)

    ## the file name changes by day, but it matches this pattern:
    se.files <- list.files(
        download.dir,
        "Antal nyinskrivna v*",
        full.names=TRUE
    )
    message( "found these files: " )
    message( paste( se.files ) )
    
    ## we get the last file in case of multiple, stale files
    se.file <- se.files[ length(se.files) ]
    message( paste("will read from", se.file ) )

    se.xlsx <- read_xlsx( se.file )
    n <- nrow(se.xlsx)

    se <- as.data.table( se.xlsx[ 2:n, c(1,3) ] )
    setnames( se, c("Datum","...3"), c("Day","Count") )
    se$Day <- as.Date( se$Day )
    se$Count <- cumsum( se$Count )
    se$Reg1 <- "Sweden"
    se$Reg2 <- "All"
    se$Reg3 <- "All"
    se$What <- "ICU Admissions"
    setorder( se, Day )
    
    ## remove files. we shamelessly bet there is nothing else starting
    ## mathcing the pattern above:
    sapply( se.files, unlink )

    ## we also write out the data so that we have a cached copy in
    ## case getData.R is called non-interactively
    fwrite( se, "covid_swedish_icu_admissions.csv" )

    se
}
