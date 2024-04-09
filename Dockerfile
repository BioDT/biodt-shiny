FROM rocker/r-base:4.3.0
RUN apt-get update -y && apt-get install -y  make pandoc zlib1g-dev git libicu-dev libpng-dev libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev libxml2-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_version("renv", version = "1.0.3")'
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'
