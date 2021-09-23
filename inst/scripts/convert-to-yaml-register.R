#!/usr/local/bin/Rscript
#################################################################
##                                                             ##
##  Convert yaml files to JSON schema and register in Synapse  ##
##                                                             ##
#################################################################

doc <- "Convert yaml schemas to JSOn and register Schemas on Synapse
Usage:
  convert-to-yaml-register.R [--dryRun]
  convert-to-yaml-register.R <files>... [--dryRun]
  convert-to-yaml-register.R (-h | --help)
  convert-to-yaml-register.R --version
Options:
  -h --help    Show this screen.
  --version    Show version.
  --dryRun     Tests if the schema(s) can be registered, but does not actually
               register them.
"

library("docopt")
library("schemann")

opts <- docopt(
  doc,
  version = 'Convert yaml to JSON Schemas and Register in Synapse 0.1\n'
)

synapse <- reticulate::import("synapseclient")
syn <- synapse$Synapse()
syn$login()

if (length(opts$files) == 0) {
  message("No files provided; nothing to convert and register!")
  quit(status = 0)
}

## New files that need registered should be passed in
# test files
files <- list.files(
  "~/sysbioDCCjsonschemas/schema_metadata_templates",
  pattern = ".yml",
  full.names = TRUE,
  recursive = TRUE
)

## Grab schema table for current synapseAnnotation versions
## Write anchors file; it's fine if there are more anchors than needed -- the
## import of the yaml file will only dereference ones that exist in the schema
synann_table <- "syn26050066"
anchor_path <- schemann::get_anchors_from_table(
  syn = syn,
  anchor_table = synann_table
)

## Combine yaml anchor file and each of the new/newly changed yaml schemas

## Convert each combined anchor/yaml schema file to json

## Register each json schema
