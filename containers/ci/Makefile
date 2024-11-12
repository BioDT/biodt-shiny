IMAGE_ROOT?=ghcr.io/biodt
IMAGE=shiny-ci
IMAGE_VERSION=0.1.0

build: Dockerfile renv.lock
	docker buildx build --platform linux/amd64 \
		--label "org.opencontainers.image.source=https://github.com/BioDT/biodt-shiny" \
		--label "org.opencontainers.image.description=BioDT Shiny CI environment" \
		-t ${IMAGE_ROOT}/${IMAGE}:${IMAGE_VERSION} \
		.

push:
	docker push ${IMAGE_ROOT}/${IMAGE}:${IMAGE_VERSION}

renv.lock: ../../renv.lock
	cp -v $< $@
