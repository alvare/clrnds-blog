all: build

build: blog
	stack exec blog build

blog: Main.hs
	stack build

new:
	@./new_post.sh

watch:
	stack exec blog watch

clean:
	stack exec blog clean

publish: build
	@rsync -azP _site/ linoder@clrnd.com.ar:/var/www/blog
