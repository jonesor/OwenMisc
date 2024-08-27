#timeline

library(googleAuthR)
library(googleway)

gauth <- googleAuthR::gar_auth(email = "jonesor@gmail.com")

location_history <- googleway::google_timeline(auth = gauth)
