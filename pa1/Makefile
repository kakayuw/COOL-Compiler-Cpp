SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

ASSN = 1
CLASS= csgy6413
SRC= stack.cl README 
LSRC= Makefile atoi.cl stack.test
COOLC= ../refimpl/coolc
SUBMISSIONFILES= stack.cl README
ZIPFILE=pa${ASSN}-submission.zip

.PHONY: clean default zip
default: test

${COOLC}:
	$(error Please copy the reference implementation of the compiler into your project directory, or modify the COOLC variable to point to it)

test: stack.cl atoi.cl ${COOLC}
	@${COOLC} -r stack.cl atoi.cl < stack.test

prep:
	rm -f ../pa${ASSN}.zip
	zip ../pa${ASSN}.zip `find * -type d -prune -o -type f -print`

clean :
	rm -f *.s core *~

zip:
	@rm -f ${ZIPFILE}
	@zip -MM ${ZIPFILE} ${SUBMISSIONFILES}
	@echo "Zipped files into ${ZIPFILE}"
