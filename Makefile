RSCRIPT = Rscript
SCRIPT_FILE = ./R/build.R

all: build

build clean open publish:
	$(RSCRIPT) $(SCRIPT_FILE) $@ 

test: build open
