#1/bin/sh
fpdoc --descr=thorium.xml --package=thoriumcorepkg --format=latex --input=../thorium.pas
pdflatex main.tex
