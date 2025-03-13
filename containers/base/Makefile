IMAGE_ROOT?=ghcr.io/biodt
IMAGE=shiny-base
IMAGE_VERSION=0.5.0

build: Dockerfile renv.lock
	docker buildx build --platform linux/amd64 \
		--label "org.opencontainers.image.source=https://github.com/BioDT/biodt-shiny" \
		--label "org.opencontainers.image.description=BioDT Shiny base environment" \
		-t ${IMAGE_ROOT}/${IMAGE}:${IMAGE_VERSION} \
		.
	docker image tag ${IMAGE_ROOT}/${IMAGE}:${IMAGE_VERSION} ${IMAGE_ROOT}/${IMAGE}:latest

push:
	docker push ${IMAGE_ROOT}/${IMAGE}:${IMAGE_VERSION}
	docker push ${IMAGE_ROOT}/${IMAGE}:latest

renv.lock: ../../renv.lock
	cp -v $< $@
