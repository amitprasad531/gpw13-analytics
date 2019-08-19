shinyServer(function(input, output, session) {

source("server/maps.R", local = TRUE)
  
source("server/database.R", local = TRUE)

source("server/metadata.R", local = TRUE)
  
})
