#!/bin/bash

make html
cp -r _build/html/* $1
cd $1
git add .
git commit -a -m "Updated site."
git push 
