elm.min.js: elm.js
	closure-compiler --js $< --js_output_file $@

elm.js: Main.elm
	elm make --optimize --output=$@ $<
