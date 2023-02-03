SOUNDFONTS=public/acoustic_grand_piano-mp3.js

build: $(shell find src) public/* node_modules $(SOUNDFONTS)
	mkdir -p build
	npx shadow-cljs release app
	rsync -aLz --exclude js --exclude '.*.swp' public/ build
	touch build

$(SOUNDFONTS):
	wget https://raw.githubusercontent.com/gleitz/midi-js-soundfonts/gh-pages/FluidR3_GM/acoustic_grand_piano-mp3.js -O $@

node_modules: package.json
	pnpm i --no-lockfile --shamefully-hoist

.PHONY: watch clean

watch: node_modules public/acoustic_grand_piano-mp3.js
	npm run watch

repl: node_modules
	npx shadow-cljs cljs-repl app

clean:
	rm -rf build

