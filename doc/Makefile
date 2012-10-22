jekyll=bundle exec jekyll

all:
	$(jekyll)

watch:
	$(jekyll) --auto

setup: pygments
	bundle install

pygments:
	git clone https://github.com/sol/pygments -b hspec-doc

clean:
	-rm -r _site
	-rm -r .cache
