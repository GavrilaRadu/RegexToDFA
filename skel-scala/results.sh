#!/bin/bash

function run_scala() {
	bonus=1.25
	result_op="$(sbt test | grep "+" | grep -o "[0-9]\+p" | grep -o "[0-9]\+" | tr '\n' '+')0"
	score=$(echo $result_op | bc)
}

function run_python() {
	bonus=1
	result_op="$(python3 -m unittest 2> /dev/null | grep -o "[0-9]\+p" | grep -o "[0-9]\+" | tr '\n' '+')0"

	score=$(echo $result_op | bc)
}

MAX_SCORE=78

run_scala

echo $score/$MAX_SCORE
echo "scale = 2; $score*$bonus/$MAX_SCORE" | bc