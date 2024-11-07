# Container images

General workflow for container images is:
1. Image is built on a local computer using Podman/Docker
2. Image is pushed to GitHub container registry (ghcr.io)

For each directory, there are two files:
- `Dockerfile`: This is the recipe for building a container image
- `Makefile`: This is a helper file for simplifying building the image (so that one can say `make build` instead of `docker buildx build --platform ... --build-arg ...`)


## First-time setup on Ubuntu

Install docker or podman:

    sudo apt install podman-docker

Add the following environment variable to `~/.bashrc` or redefine it always before running build commands:

    export BUILDAH_FORMAT=docker


## Updating and building a new image

Typical workflow is:

1. **Important:** Update `IMAGE_VERSION` variable in `Makefile`. If this is not done, an existing image with the same version gets overwritten, which is problematic for reproducibility.

2. Update `Dockerfile` and/or `Makefile` as needed.

3. Build a new image:

       make build

3. Login to GitHub container registry.
   Use GitHub username and Personal Access Token with scope 'write:packages' as username and password, respectively.
   See [these instructions for creating a token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token#creating-a-personal-access-token-classic).

       docker login ghcr.io

4. Push the image to ghcr.io:

       make push


## Using the built image via docker/podman

You can test the image locally before pushing it to ghcr.io.

Example for running bash with the image:

    docker run -it --rm --entrypoint bash IMAGE_NAME:IMAGE_VERSION

