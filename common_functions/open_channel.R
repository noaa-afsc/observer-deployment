# channel_function - Function to facilitate connection to AFSC/FMA tables and views 
#                       from within the AFSC/FMA (Seattle) and at the AKRO-SF (Juneau)
open_channel <- function(db = "AFSC"){
  
  location <- toupper(rstudioapi::showPrompt("LocationL", 'What is your current physical location? (Juneau or Seattle)'))
  if(!location %in% c("SEATTLE", "JUNEAU")){
    cat(paste0('Location \'', location, '\' not recognized. Must be Seattle or Juneau.'))
  }
  uid <- rstudioapi::askForPassword("Database user")
  pwd <- rstudioapi::askForPassword("Database password")
  
  if(location == 'SEATTLE'){
    if(!require("odbc"))   install.packages("odbc", repos = 'http://cran.us.r-project.org')
    channel <- dbConnect(drv = odbc::odbc(),"AFSC", UID = uid, PWD = pwd)
  }
  
  if(location == 'JUNEAU' & db == 'AFSC'){
    if(!require("ROracle"))   install.packages("ROracle", repos = 'http://cran.us.r-project.org')
    channel <- dbConnect(
      drv = dbDriver('Oracle'), username = uid, password = pwd,
      dbname = "(DESCRIPTION =(ADDRESS = (PROTOCOL = TCP)(HOST = raja.afsc.noaa.gov)(PORT = 1521))(CONNECT_DATA =(SERVER = DEDICATED)(SID = afscp1)))")
  }
  
  if(location == 'JUNEAU' & db == 'AKRO'){
    if(!require("ROracle"))   install.packages("ROracle", repos='http://cran.us.r-project.org')
    channel <- dbConnect(
      drv = dbDriver('Oracle'), username = uid, password = pwd,
      dbname = "(DESCRIPTION =(ADDRESS = (PROTOCOL = TCP)(HOST = akr-j50.nmfs.local)(PORT = 2040))(CONNECT_DATA =(SERVER = DEDICATED)(service_name = AKR1)))")
  }
  
  cat("Use Syntax:\n<SQL QUERY OBJECT NAME> <- paste(<your sql query>) \n<YOUR OBJECT FROM QUERY> <- dbGetQuery(channel, <QUERY OBJECT NAME>)")
  channel
  
}
