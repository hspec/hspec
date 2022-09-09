jekyll=./bundle exec jekyll
export PATH := bin:$(PATH)

all: setup
	$(jekyll) build
	sed -i 's/[ \t]*$$//' _site/*.html

watch: setup
	$(jekyll) build --watch

serve: setup
	$(jekyll) serve --host=0.0.0.0

setup: bin/hspec-discover

bin/hspec-discover:
	./bundle install
	rm -f .ghc.environment.*
	cabal install base deepseq transformers HUnit QuickCheck hspec hspec-api hspec-contrib --lib --package-env .
	cabal install --installdir=bin hspec-discover

clean:
	-rm -rf _site
	-rm -rf _cache
	-rm -rf bin
