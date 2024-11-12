# -*- Makefile -*-

BUILD_DIR := builds
PKG_NAME := jaspMissingData

all: build

## Build the package:
build: R/* roxygen | $(BUILD_DIR)
	R CMD build ./ > $(BUILD_DIR)/$(PKG_NAME).tar.gz

## Run unit tests:
test: tests/* roxygen
	R CMD check ./

## Build and install the package via renv:
renv:
	Rscript ./renv_build.R

## Update the NAMESPACE and help files:
roxygen:
	Rscript --vanilla -e "roxygen2::roxygenize(clean = TRUE)"

## Make sure we have a build directory:
$(BUILD_DIR):
	mkdir $(BUILD_DIR)

.PHONY: $(BUILD_DIR) roxygen renv
