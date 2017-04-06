jekyll=tinc exec bundle exec jekyll

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
	bundle install
	tinc

clean:
	-rm -rf _site
	-rm -rf .cache
