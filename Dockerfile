FROM ghcr.io/biodt/shiny-base:latest

ARG UC_GRASSLAND_MODEL_REPO=https://github.com/BioDT/uc-grassland-model.git
ARG UC_GRASSLAND_MODEL_REF=main

ENV UC_GRASSLAND_MODEL_DIR=/opt/uc-grassland-model
ENV PATH=${UC_GRASSLAND_MODEL_DIR}/build:${PATH}

RUN apt-get update -qy && \
    apt-get install -qy \
        cmake \
        g++ \
        python3 \
        python3-dev \
        && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN git clone --depth 1 --branch "${UC_GRASSLAND_MODEL_REF}" "${UC_GRASSLAND_MODEL_REPO}" "${UC_GRASSLAND_MODEL_DIR}" && \
    cmake -S "${UC_GRASSLAND_MODEL_DIR}" -B "${UC_GRASSLAND_MODEL_DIR}/build" && \
    cmake --build "${UC_GRASSLAND_MODEL_DIR}/build" --target GRASSMIND3 --parallel "$(nproc)" && \
    ln -sf "${UC_GRASSLAND_MODEL_DIR}/build/GRASSMIND3" /usr/local/bin/GRASSMIND3

# Copy application code
COPY . .

EXPOSE 7860

# Override the entrypoint of the base image
ENTRYPOINT ["/bin/bash"]

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
