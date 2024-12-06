# start from the rstudio/plumber image
FROM rstudio/plumber

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev  libpng-dev pandoc 
    
    
# install plumber
RUN R -e "install.packages(c('plumber', 'tidyverse', 'tidymodels', 'ranger', 'parsnip'))"

# copy API.R from the current directory into the container
COPY API.R API.R

# open port to traffic
EXPOSE 9759

# when the container starts, start the myAPI.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('API.R'); pr$run(host='0.0.0.0', port=9759)"]
