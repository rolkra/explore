# Create a data dictionary Markdown file

Create a data dictionary Markdown file

## Usage

``` r
data_dict_md(
  data,
  title = "",
  description = NA,
  output_file = "data_dict.md",
  output_dir
)
```

## Arguments

- data:

  A dataframe (data dictionary for all variables)

- title:

  Title of the data dictionary

- description:

  Detailed description of variables in data (dataframe with columns
  'variable' and 'description')

- output_file:

  Output filename for Markdown file

- output_dir:

  Directory where the Markdown file is saved

## Value

Create Markdown file

## Examples

``` r
# Data dictionary of a dataframe
data_dict_md(iris,
             title = "iris flower data set",
             output_dir = tempdir())

# Data dictionary of a dataframe with additional description of variables
description <- data.frame(
                 variable = c("Species"),
                 description = c("Species of Iris flower"))
data_dict_md(iris,
             title = "iris flower data set",
             description = description,
             output_dir = tempdir())
```
