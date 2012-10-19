jekyll=jekyll/bin/jekyll

all: jekyll
	$(jekyll)

watch: jekyll
	$(jekyll) --auto

jekyll: pygments
	git clone https://github.com/sol/jekyll -b hspec-doc
	cd jekyll && bundle install


pygments:
	git clone https://github.com/sol/pygments -b hspec-doc

clean:
	rm -rf jekyll
	rm -rf pygments
	rm -r _site
