#!/bin/bash

NAME="regions_Dutch-HUG-Day_2010"

lhs2TeX -o ${NAME}.tex ${NAME}.lhs

pdflatex ${NAME}.tex
