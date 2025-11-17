FROM docker.io/rocker/r-ver:4.4.1

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -qy && \
    apt-get install -qy \
        curl \
        gdal-bin \
        gdal-data \
        git \
        libcurl4-openssl-dev \
        libgdal-dev \
        libgeos-dev \
        libicu-dev \
        libpng-dev \
        libproj-dev \
        libsqlite3-dev \
        libssl-dev \
        libudunits2-dev \
        libxt6 \
        make \
        pandoc \
        zlib1g-dev \
        && \
    apt-get clean

# R packages
RUN Rscript -e 'install.packages("remotes")' && \
    Rscript -e 'remotes::install_version("renv", version = "1.0.7")'

COPY renv.lock /biodt/renv.lock
RUN cd /biodt && \
    Rscript -e 'renv::restore()'

# Node
ARG NODE_VERSION=22.11.0
RUN mkdir -p /opt && \
    cd /opt && \
    curl -O https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-x64.tar.xz && \
    curl -O https://nodejs.org/dist/v$NODE_VERSION/SHASUMS256.txt && \
    sha256sum -c SHASUMS256.txt --ignore-missing && \
    tar xf node-v$NODE_VERSION-linux-x64.tar.xz && \
    rm node-v$NODE_VERSION-linux-x64.tar.xz SHASUMS256.txt && \
    ln -s node-v$NODE_VERSION-linux-x64 node
ENV PATH=/opt/node/bin:$PATH

# Dependencies for Cypress
RUN apt-get update -qy && \
    apt-get install -qy \
        libgtk2.0-0 \
        libgtk-3-0 \
        libgbm-dev \
        libnotify-dev \
        libnss3 \
        libxss1 \
        libasound2 \
        libxtst6 \
        xauth \
        xvfb \
        && \
    apt-get clean

# Docker
RUN apt-get update -qy && \
    apt-get install -qy \
        docker.io \
        && \
    apt-get clean

# Install uc-ces-recreation package since renv has not captured it
# This may be due to the fact, it is missing main branch
RUN Rscript -e 'renv::install("biodt/uc-ces-recreation")'
# Entrypoint script
# COPY run_shiny.sh /biodt/run_shiny.sh
# RUN chmod a+x /biodt/run_shiny.sh
ENTRYPOINT ["bash"]
