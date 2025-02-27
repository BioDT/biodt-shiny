FROM ghcr.io/biodt/shiny-base:latest

# Copy application code
COPY . .

EXPOSE 7860

# Override the entrypoint of the base image
ENTRYPOINT ["/bin/bash"]

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
