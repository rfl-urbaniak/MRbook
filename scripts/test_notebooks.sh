#!/bin/bash


#TODO modify when notebooks land
INCLUDED_NOTEBOOKS="docs/weight/missing_fingers.ipynb"

EXCLUDED_NOTEBOOK=""
 
CI=1 python -m pytest -v --nbval-lax --dist loadscope -n auto $INCLUDED_NOTEBOOKS --ignore=$EXCLUDED_NOTEBOOK
