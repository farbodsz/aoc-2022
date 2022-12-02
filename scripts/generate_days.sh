#!/usr/bin/env sh

INPUT_FILE=./template/DayXX.hs
OUTPUT_DIR=../src/

for i in $(seq -w 1 24); do
  sed "s/DayXX/Day$i/" "$INPUT_FILE" >"$OUTPUT_DIR/Day$i.hs"
done
