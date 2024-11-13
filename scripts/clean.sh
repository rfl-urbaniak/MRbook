#!/bin/bash
set -euxo pipefail

isort --profile="black" probability_on_trial/ tests/
autoflake --remove-all-unused-imports --in-place --recursive ./probability_on_trial  ./tests
nbqa --nbqa-shell isort --profile="black" docs/
nbqa --nbqa-shell autoflake  --nbqa-shell --remove-all-unused-imports --recursive --in-place docs/  
black probability_on_trial/ tests/ docs/
