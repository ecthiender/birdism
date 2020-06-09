project  := birdism
registry := rayanon

project_dir := $(shell pwd)/server
cabal_file  := $(project_dir)/$(project).cabal
build_dir   := $(project_dir)/$(shell stack path --dist-dir)/build
version     := v$(shell grep -oP '^version:\s*\K.*' $(cabal_file))

image: $(cabal_file)
	cd $(project_dir) && stack build
	cp $(build_dir)/$(project)/$(project) ./
	strip ./$(project)
	upx ./$(project)
	docker build --build-arg project_bin=./$(project) -t $(registry)/$(project):$(version) .
	rm ./$(project)

push:
	docker push $(registry)/$(project):$(version)

.PHONY: push image
