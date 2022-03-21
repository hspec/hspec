jekyll=./bundle exec jekyll

all:
	$(jekyll) build

watch:
	$(jekyll) build --watch

serve:
	$(jekyll) serve --host=0.0.0.0


# only generate css + copy js
less:
	lessc css/custom.less > _site/css/custom.css
	cp js/*.js _site/js

watch-less: less
	watchr -e 'watch("(^js/.*\.js)|(^css/.*\.less)") {system "make less"};'

setup:
	rm -rf vendor
	./bundle install
	ln -s /usr/bin/python2 vendor/bundle/bin/python
	rm -f .ghc.environment.*
	cabal install HUnit QuickCheck hspec hspec-contrib --lib --package-env .

clean:
	-rm -rf _site
	-rm -rf .cache
