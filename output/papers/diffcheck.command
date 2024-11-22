#!/bin/bash
cd -- "$(dirname "$BASH_SOURCE")"

# change the paths to your respective tex files
latexdiff --disable-auto-mbox 'JASA - Round 1/Eng-vowel-norm-wo-SI.tex' Eng-vowel-norm.tex > manuscript_diff.tex
sleep 20
xelatex -interaction=batchmode -halt-on-error manuscript_diff.tex
bibtex manuscript_diff
xelatex -interaction=batchmode -halt-on-error manuscript_diff.tex

mv manuscript_diff.* 'JASA - Round 2/'
