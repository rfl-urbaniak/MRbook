
from setuptools import find_packages, setup

VERSION = "0.1.0"



# examples/tutorials/notebooks
DEV_REQUIRE = [
            "pytest==7.4.3",
            "pytest-cov",
            "pytest-xdist",
            "mypy",
            "black==24.8.0",
            "flake8",
            "isort==5.13.2",
            "nbval",
            "nbqa==1.9.0",
            "autoflake",
            "sphinx==7.1.2",
            "sphinxcontrib-bibtex",
            "sphinxcontrib-jquery",
            "sphinx_rtd_theme==1.3.0",
            "nbsphinx",
            "myst_parser", 
            "pandoc",
        ]   

setup(
    name="probability_on_trial",
    version=VERSION,
    description="Computations associated with the <<Probability on trial>> book.",
    packages=find_packages(include=["probability_on_trial", "probability_on_trial.*"]),
    author="Basis",
    url="",
    project_urls={
        "Documentation": "",
        "Source": "",
    },
    install_requires=[
        "chirho @ git+https://github.com/BasisResearch/chirho.git#egg=chirho",
        "pyro-ppl>=1.8.6", "pandas==2.2.3", "plotly", "plotly.express", 
        "torch", "scipy", "scikit-learn",
        "matplotlib>=3.8.2", "dill", "torchdiffeq",
        "numpy==1.25", # the latest compatible with current pytorch
        "jupyter", "graphviz", "matplotlib", "seaborn", "kaleido",
    ],
    extras_require={
        "dev": DEV_REQUIRE,
        },
    python_requires=">=3.10, <3.12",
    keywords="probability, statistics, bayesian, pyro",
    license="Apache 2.0",
)