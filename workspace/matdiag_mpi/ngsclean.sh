#!/bin/sh

rm -f $1 $2

globusrun -r grid-compute.leeds.ac.uk -f ngsclean.rsl 