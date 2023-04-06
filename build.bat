@echo off
elm make src/Main.elm --output=docs/elm-wireworld.js

if exist "index.html" (
  del index.html
  echo [LOG]./index.html was deleted.
)
