.PHONY: doc clean cabal ubuntu-build-deps tarballs README

# Prevents the .sha files from being deleted. Not sure what the story is.
.SECONDARY:

v = $(shell git describe || bin/tags cabal_version || echo '(unversioned)')
tag = taskl-$(shell ./bin/tags tag)
built = $(wildcard tmp/dist/*/taskl)
tarballs = $(built:%/arx=%.tbz)

ifeq (Darwin,$(shell uname))
  tagged = tmp/taskl.cabal
else
  tagged = tmp/taskl.custom
endif

this_platform: tmp/dist/$(tag)/taskl

version:
	echo $v > $@

tmp/dist/$(tag)/taskl: $(tagged)
	mkdir -p tmp/dist/$(tag)
	mv -f $< $@

tmp/dist/%/taskl.gpg: tmp/dist/%/taskl
	gpg --use-agent --detach-sign $<

tmp/dist/%/taskl.sha: d = $(@:%/taskl.sha=%)
tmp/dist/%/taskl.sha: tmp/dist/%/taskl
	( cd $d && shasum --portable --algorithm 512 taskl > taskl.sha )

tarballs: $(tarballs)

tmp/dist/%.tbz: d = $(@:tmp/dist/%.tbz=%)
tmp/dist/%.tbz: tmp/dist/%/taskl tmp/dist/%/taskl.gpg tmp/dist/%/taskl.sha
	tar cjf $@ -C tmp/dist $d

taskl: version taskl.hs doc
	ghc -outputdir tmp --make -O2 taskl.hs -o taskl

tmp/taskl.custom: libs = $(shell bin/so2a4hs statics taskl)
tmp/taskl.custom: taskl bin/so2a4hs
	ghc -outputdir tmp --make -O2 taskl.hs -o $@ \
	 -optl-Wl,--whole-archive \
	  $(libs:%=-optl%) \
	 -optl-Wl,--no-whole-archive
	strip $@

ubuntu-build-deps:
	env DEBIAN_FRONTEND=noninteractive aptitude install -y cabal-install
	cabal install --only-dependencies

tmp/taskl.cabal: dist/build/taskl/taskl
	mkdir -p tmp
	cp dist/build/taskl/taskl $@
	strip $@

dist/build/taskl/taskl: cabal

cabal: version
	cabal configure --disable-executable-profiling \
	                --disable-library-profiling
	cabal build

clean:
	rm -rf tmp taskl dist/build

README:
	( cd doc && make man )
	man doc/.build/man/taskl.1 | col -bx > doc/plain
	sed -n '/SYNOPSIS/,/AUTHOR/ { /AUTHOR/d ; p ;}' < doc/plain > README

