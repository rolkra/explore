# Generate a notebook

Generate an RMarkdown Notebook template for a report. You must provide a
output-directory (parameter output_dir). The default file-name is
"notebook-explore.Rmd" (may overwrite existing file with same name)

## Usage

``` r
create_notebook_explore(output_file = "notebook-explore.Rmd", output_dir)
```

## Arguments

- output_file:

  Filename of the html report

- output_dir:

  Directory where to save the html report

## Examples

``` r
create_notebook_explore(output_file = "explore.Rmd", output_dir = tempdir())
#> Notebook /tmp/RtmpyvqPrd/explore.Rmd generated
```
