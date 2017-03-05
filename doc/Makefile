jekyll=tinc exec bundle exec jekyll build

all:
	$(jekyll)

watch:
	$(jekyll) --watch


# only generate css + copy js
less:
	lessc css/custom.less > _site/css/custom.css
	cp js/*.js _site/js

watch-less: less
	watchr -e 'watch("(^js/.*\.js)|(^css/.*\.less)") {system "make less"};'

setup: pygments
	bundle install
	tinc

pygments:
	git clone https://github.com/sol/pygments -b hspec-doc

clean:
	-rm -rf _site
	-rm -rf .cache
