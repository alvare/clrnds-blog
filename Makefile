all: build

cabal.sandbox.config:
	cabal sandbox init --sandbox=../hakyll-sandbox

build: dist/build/blog/blog
	./dist/build/blog/blog build

dist/build/blog/blog: Main.hs
	cabal build
	./dist/build/blog/blog clean

new:
	@./new_post.sh

watch: dist/build/blog/blog
	./dist/build/blog/blog watch

clean: dist/build/blog/blog
	./dist/build/blog/blog clean
