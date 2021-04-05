# get shiny server plus tidyverse packages image
FROM rocker/shiny:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    curl \
    libcurl4-openssl-dev \
    libxml2-dev \
    libudunits2-0 \
    libudunits2-dev \
    w3m

# some of these are needed for the r sf package to run
RUN apt-get update && apt-get install -y \
	libgdal-dev \
	libgeos-dev \
	libproj-dev \
	libfontconfig1-dev \
	xdg-utils

# Install R Dependencies - installs the r packages you need - if this step fails you’re likely
# missing system libraries that a package requires
RUN install2.r --error \
    # ofers dependencies
    shiny \
    ggplot2 \
    tidyverse \
    plotly \
    DT \
    shinythemes \
    # end ofers dependencies
	rio \
	maps \
	sp \
	maptools \
	housingData \
	leaflet \
	DT\
	shinycustomloader

RUN install2.r --error \
	sf

### -----------------------------------

# copy the contents of app folder to image
COPY app ./app

# select port
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
