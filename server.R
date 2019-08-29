shinyServer(function(input, output, session) {

source("server/correlations.R", local = TRUE) 
  
source("server/trends.R", local = TRUE)  

source("server/maps.R", local = TRUE)
  
source("server/database.R", local = TRUE)

source("server/metadata.R", local = TRUE)
  
})
