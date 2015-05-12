all: build

build: dist/build/blog/blog
	./dist/build/blog/blog build

dist/build/blog/blog: Main.hs
	cabal build

new:
	@./new_post.sh

watch: dist/build/blog/blog
	./dist/build/blog/blog watch

clean:
	./dist/build/blog/blog clean

publish:
	@rsync -azP _site/ linoder@clrnd.com.ar:/var/www/blog
