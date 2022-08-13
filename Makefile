project  := birdism
registry := rayanon

server_dir := $(shell pwd)/server
ui_dir     := $(shell pwd)/gooey
cabal_file := $(server_dir)/$(project).cabal
build_dir  := $(server_dir)/$(shell cd server && stack path --no-install-ghc --dist-dir)/build
version    := v$(shell grep -oP '^version:\s*\K.*' $(cabal_file))

image: $(cabal_file)
	cd $(server_dir) && stack build
	cp $(build_dir)/$(project)/$(project) ./
	strip ./$(project)
	upx ./$(project)
	cd $(ui_dir) && yarn build
	docker build --build-arg project_bin=./$(project) -t $(registry)/$(project):$(version) .
	rm ./$(project)

push:
	docker push $(registry)/$(project):$(version)

.PHONY: push image
