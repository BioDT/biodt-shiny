FROM rocker/r-ver:4.3.3

RUN apt-get update -y && apt-get install -y  make pandoc zlib1g-dev libcurl4-openssl-dev libssl-dev libicu-dev libpng-dev libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev git libudunits2-dev libxt6 && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/

RUN mkdir /.cache
RUN chmod 777 /.cache .
WORKDIR /code

RUN echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_version("renv", version = "1.0.7")'
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'

# Copy application code
COPY . .

EXPOSE 7860

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
