#!/bin/bash
set -euxo pipefail

mypy --ignore-missing-imports probability_on_trial/  tests/
isort --check --profile="black" --diff probability_on_trial/ tests/
black --check probability_on_trial/ tests/ docs/
flake8 probability_on_trial/ tests/
nbqa --nbqa-shell autoflake --nbqa-shell --recursive --check docs/
