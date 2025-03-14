#!/bin/bash
# get script directory
CURDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# change to the docs directory
cd $CURDIR/..
cd docs
sphinx-quickstart . -r "1.0.0" \
    -l "en" -p "Probability on Trial" \
    --no-sep -a "Urbaniak, Di Bello and Lewandowski" \
    --ext-autodoc --ext-mathjax --ext-intersphinx --ext-imgmath --ext-viewcode \
    --extensions nbsphinx --extensions sphinxcontrib.bibtex --extensions myst_parser \
    --extensions sphinxcontrib.jquery

