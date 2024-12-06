# start from the rstudio/plumber image
FROM rstudio/plumber

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev  libpng-dev pandoc 
    
    
# install needed packages
RUN R -e "install.packages(c('plumber', 'tidyverse', 'tidymodels', 'ranger', 'parsnip'))"

# Set the working directory to /app
WORKDIR /app

# Copy the API script and the dataset into the container's working directory
COPY API.R API.R

COPY diabetes_binary_health_indicators_BRFSS2015.csv diabetes_binary_health_indicators_BRFSS2015.csv

# open port to traffic
EXPOSE 9759

# when the container starts, start the API.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('API.R'); pr$run(host='0.0.0.0', port=9759)"]
