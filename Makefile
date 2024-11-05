# -*- Makefile -*-

BUILD_DIR := builds

all: build

## Build the package:
build: R/* roxygen | $(BUILD_DIR)
	R CMD build ./ > $(BUILD_DIR)/jaspImputation.tar.gz

## Run unit tests:
test: tests/* roxygen
	R CMD check ./

## Update the NAMESPACE and help files:
roxygen:
	Rscript -e "roxygen2::roxygenize(clean = TRUE)"

## Make sure we have a build directory:
$(BUILD_DIR):
	mkdir $(BUILD_DIR)

.PHONY: roxygen
