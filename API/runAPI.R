#Run my API

library(plumber)
r <- plumb("API/API.R")

# Run on the port in the Dockerfile
r$run (port = 9759)


