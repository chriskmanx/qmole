DATADIR := data
ATTRIBUTES_INDEX := \
	-a toc \
	-a linkcss \
	-a stylesdir=$(DATADIR) \
	-a scriptsdir=$(DATADIR) \
	-a icons

all: index.html

index.html: index.txt
	@echo "compiling $<..."; asciidoc $(ATTRIBUTES_INDEX) $<

clean: 
	rm -rf index.html
