#!/bin/bash
set -euxo pipefail

python -m pytest -s --cov=probability_on_trial/ --cov=tests --cov-report=term-missing ${@-} --cov-report=html:tests/coverage
