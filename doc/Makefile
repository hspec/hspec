jekyll=jekyll/bin/jekyll

all: setup
	$(jekyll)

watch: setup
	$(jekyll) --auto

setup: jekyll pygments
	mkdir -p .cache/ghc
	mkdir -p .cache/runhaskell

jekyll: pygments
	git clone https://github.com/sol/jekyll -b hspec-doc
	cd jekyll && bundle install


pygments:
	git clone https://github.com/sol/pygments -b hspec-doc

clean:
	-rm -r _site
	-rm -r .cache
