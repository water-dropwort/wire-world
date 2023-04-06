@echo off
elm make src/Main.elm --output=webpage/elm-wireworld.js

if exist "index.html" (
  del index.html
  echo [LOG]./index.html was deleted.
)
