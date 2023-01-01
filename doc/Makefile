jekyll=./bundle exec jekyll

all: setup
	$(jekyll) build

watch: setup
	$(jekyll) build --watch

serve: setup
	$(jekyll) serve --host=0.0.0.0

setup: bin/hspec-discover

bin/hspec-discover:
	./bundle install
	rm -f .ghc.environment.*
	cabal install HUnit QuickCheck hspec hspec-contrib --lib --package-env .
	cabal install --installdir=bin hspec-discover

clean:
	-rm -rf _site
	-rm -rf _cache
	-rm -rf bin
