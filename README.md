
# schemann

<!-- badges: start -->
<!-- badges: end -->

schemann provides functionality for interacting with the Synapse JSON schema
API services. These services allow for registering and validating Synapse entity
annotations using a JSON schema.

## Installation

schemann requires reticulate and the [Synapse Python client](https://pypi.org/project/synapseclient/). See the
[reticulate documentation](https://rstudio.github.io/reticulate/#python-version)
for information on how to set R to use a specific version of Python if you don’t
want to use the default Python installation on your machine. Whichever Python 
installation you choose should have `synapseclient` installed.

Because schemann uses reticulate, it is not compatible with the
[synapser](https://r-docs.synapse.org/) package.

You can install the development version of schemann with:

``` r
remotes::install_github("Sage-Bionetworks/schemann")
```
