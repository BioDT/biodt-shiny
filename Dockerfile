FROM rocker/verse:4.3.0
RUN apt-get update && apt-get install -y  gdal-bin libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libicu-dev libpng-dev libproj-dev libssl-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("fs",upgrade="never", version = "1.6.3")'
RUN Rscript -e 'remotes::install_version("jsonlite",upgrade="never", version = "1.8.7")'
RUN Rscript -e 'remotes::install_version("stringi",upgrade="never", version = "1.7.12")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("terra",upgrade="never", version = "1.7-46")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.5")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.29")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.10")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "NA")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinipsum",upgrade="never", version = "0.1.0")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.4")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.2.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_github("rstudio/bslib@b9ac2cfc0f69e08714648f8263b027fb7e9e8511")'
RUN Rscript -e 'remotes::install_github("rstudio/htmltools@6dddb5861c04b5e988b839ceda23a23af3d7b6df")'
RUN Rscript -e 'remotes::install_github("daattali/shinycssloaders@62815f7af20df73e1dfd561ce9a9460f73907488")'
RUN Rscript -e 'remotes::install_github("it4innovations/rtus")'
RUN Rscript -e 'remotes::install_github("it4innovations/r4lexis")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');library(BioDTShiny);BioDTShiny::run_app()"]
