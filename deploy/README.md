# Build Shiny container

```bash
docker build -f Dockerfile_base --progress=plain -t biodtshiny_base .
docker build -f Dockerfile --progress=plain -t biodtshiny:latest .
docker run -p 3838:3838 biodtshiny:latest
# then go to 127.0.0.1:3838
```

# Build Shiny container with Beehave

Continue building from the previous steps:

```bash
docker login ghcr.io  # login to pull private beehave container
docker build -f Dockerfile_beehave --progress=plain -t biodtshiny_beehave:latest .
docker run -p 3838:3838 biodtshiny_beehave:latest
```

This container defines executable `beehave_Rscript` that runs `Rscript` in Beehave R environment.
Example use:

```bash
git clone git@github.com:BioDT/uc-beehave-execution-scripts.git
cd uc-beehave-execution-scripts
mkdir data/output

docker run -it --rm --entrypoint bash -v "$(pwd)":/wd:Z biodtshiny_beehave:latest
cd /wd
export MODEL_PATH="data/Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo"
beehave_Rscript R/run_beehave.R "$(cat data/single_execution.json)"
```
