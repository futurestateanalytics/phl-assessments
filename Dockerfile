FROM rocker/r-ver:4.4

# Install required system dependencies
RUN apt-get update && apt-get install -y \
    r-base-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    build-essential \
    gdal-bin \
    libgdal-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libudunits2-dev \
    && apt-get clean

RUN R -e "install.packages('renv')"

COPY . /app

WORKDIR /app

RUN R -e "renv::restore()"

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('./app.R', host='0.0.0.0', port=3838)"]
