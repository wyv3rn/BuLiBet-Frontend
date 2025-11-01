build/index.html: src/*.elm Makefile
	elm make src/Main.elm --output build/index.html

deploy: build/index.html
	scp build/index.html tau:.local/state/caddy/buli
