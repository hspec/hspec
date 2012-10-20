SRC=src
DST=_site
jekyll=jekyll/bin/jekyll

all:
	$(jekyll)

watch: jekyll
	$(jekyll) --auto

jekyll:
	git clone git@github.com:sol/jekyll.git -b redcarpet-update
	cd jekyll && bundle install

clean:
	rm -rf jekyll
	rm -r $(DST)
