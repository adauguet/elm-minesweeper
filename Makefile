live:
	elm-live src/Main.elm --pushstate -p 8000 -- --output=elm.min.js

analyse:
	elm-analyse -s