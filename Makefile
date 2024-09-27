# -*- Makefile -*-

all: roxygen

## Update the NAMESPACE and help files:
roxygen:
	Rscript -e "roxygen2::roxygenize(clean = TRUE)"

.PHONY: roxygen
