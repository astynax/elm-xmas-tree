docs/elm.min.js: docs/elm.js
	closure-compiler --js $< --js_output_file $@

docs/elm.js: Main.elm
	elm make --optimize --output=$@ $<
