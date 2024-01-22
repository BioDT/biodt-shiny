FROM biodtshiny_base
COPY renv.lock.prod renv.lock
RUN R -e 'renv::restore()'
COPY BioDTShiny_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');library(BioDTShiny);BioDTShiny::run_app(golem.app.prod = FALSE,
        cwr_path = '/data')"
