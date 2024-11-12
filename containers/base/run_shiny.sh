#!/bin/bash

COMMIT=${1:-main}

echo "Fetching biodt-shiny commit $COMMIT"
git clone https://github.com/BioDT/biodt-shiny.git /biodt/biodt-shiny
cd /biodt/biodt-shiny
git checkout $COMMIT

echo "Running shiny"
Rscript -e "shiny::runApp(host='0.0.0.0', port=7860)"
