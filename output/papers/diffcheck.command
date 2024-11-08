#!/bin/bash
cd -- "$(dirname "$BASH_SOURCE")"

# change the paths to your respective tex files
latexdiff --disable-auto-mbox 'JASA - Round 1/Eng-vowel-norm.tex' Eng-vowel-norm.tex > manuscript_diff.tex
sleep 20
xelatex manuscript_diff.tex
mv manuscript_diff.pdf 'JASA - Round 2/manuscript_diff.pdf'
