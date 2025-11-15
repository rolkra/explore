# Drop all observations where expression is true

Drop all observations where expression is true

## Usage

``` r
drop_obs_if(data, expr)
```

## Arguments

- data:

  Data frame

- expr:

  Expression

## Value

Data frame

## Examples

``` r
drop_obs_if(iris, Species == "setosa")
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1            7.0         3.2          4.7         1.4 versicolor
#> 2            6.4         3.2          4.5         1.5 versicolor
#> 3            6.9         3.1          4.9         1.5 versicolor
#> 4            5.5         2.3          4.0         1.3 versicolor
#> 5            6.5         2.8          4.6         1.5 versicolor
#> 6            5.7         2.8          4.5         1.3 versicolor
#> 7            6.3         3.3          4.7         1.6 versicolor
#> 8            4.9         2.4          3.3         1.0 versicolor
#> 9            6.6         2.9          4.6         1.3 versicolor
#> 10           5.2         2.7          3.9         1.4 versicolor
#> 11           5.0         2.0          3.5         1.0 versicolor
#> 12           5.9         3.0          4.2         1.5 versicolor
#> 13           6.0         2.2          4.0         1.0 versicolor
#> 14           6.1         2.9          4.7         1.4 versicolor
#> 15           5.6         2.9          3.6         1.3 versicolor
#> 16           6.7         3.1          4.4         1.4 versicolor
#> 17           5.6         3.0          4.5         1.5 versicolor
#> 18           5.8         2.7          4.1         1.0 versicolor
#> 19           6.2         2.2          4.5         1.5 versicolor
#> 20           5.6         2.5          3.9         1.1 versicolor
#> 21           5.9         3.2          4.8         1.8 versicolor
#> 22           6.1         2.8          4.0         1.3 versicolor
#> 23           6.3         2.5          4.9         1.5 versicolor
#> 24           6.1         2.8          4.7         1.2 versicolor
#> 25           6.4         2.9          4.3         1.3 versicolor
#> 26           6.6         3.0          4.4         1.4 versicolor
#> 27           6.8         2.8          4.8         1.4 versicolor
#> 28           6.7         3.0          5.0         1.7 versicolor
#> 29           6.0         2.9          4.5         1.5 versicolor
#> 30           5.7         2.6          3.5         1.0 versicolor
#> 31           5.5         2.4          3.8         1.1 versicolor
#> 32           5.5         2.4          3.7         1.0 versicolor
#> 33           5.8         2.7          3.9         1.2 versicolor
#> 34           6.0         2.7          5.1         1.6 versicolor
#> 35           5.4         3.0          4.5         1.5 versicolor
#> 36           6.0         3.4          4.5         1.6 versicolor
#> 37           6.7         3.1          4.7         1.5 versicolor
#> 38           6.3         2.3          4.4         1.3 versicolor
#> 39           5.6         3.0          4.1         1.3 versicolor
#> 40           5.5         2.5          4.0         1.3 versicolor
#> 41           5.5         2.6          4.4         1.2 versicolor
#> 42           6.1         3.0          4.6         1.4 versicolor
#> 43           5.8         2.6          4.0         1.2 versicolor
#> 44           5.0         2.3          3.3         1.0 versicolor
#> 45           5.6         2.7          4.2         1.3 versicolor
#> 46           5.7         3.0          4.2         1.2 versicolor
#> 47           5.7         2.9          4.2         1.3 versicolor
#> 48           6.2         2.9          4.3         1.3 versicolor
#> 49           5.1         2.5          3.0         1.1 versicolor
#> 50           5.7         2.8          4.1         1.3 versicolor
#> 51           6.3         3.3          6.0         2.5  virginica
#> 52           5.8         2.7          5.1         1.9  virginica
#> 53           7.1         3.0          5.9         2.1  virginica
#> 54           6.3         2.9          5.6         1.8  virginica
#> 55           6.5         3.0          5.8         2.2  virginica
#> 56           7.6         3.0          6.6         2.1  virginica
#> 57           4.9         2.5          4.5         1.7  virginica
#> 58           7.3         2.9          6.3         1.8  virginica
#> 59           6.7         2.5          5.8         1.8  virginica
#> 60           7.2         3.6          6.1         2.5  virginica
#> 61           6.5         3.2          5.1         2.0  virginica
#> 62           6.4         2.7          5.3         1.9  virginica
#> 63           6.8         3.0          5.5         2.1  virginica
#> 64           5.7         2.5          5.0         2.0  virginica
#> 65           5.8         2.8          5.1         2.4  virginica
#> 66           6.4         3.2          5.3         2.3  virginica
#> 67           6.5         3.0          5.5         1.8  virginica
#> 68           7.7         3.8          6.7         2.2  virginica
#> 69           7.7         2.6          6.9         2.3  virginica
#> 70           6.0         2.2          5.0         1.5  virginica
#> 71           6.9         3.2          5.7         2.3  virginica
#> 72           5.6         2.8          4.9         2.0  virginica
#> 73           7.7         2.8          6.7         2.0  virginica
#> 74           6.3         2.7          4.9         1.8  virginica
#> 75           6.7         3.3          5.7         2.1  virginica
#> 76           7.2         3.2          6.0         1.8  virginica
#> 77           6.2         2.8          4.8         1.8  virginica
#> 78           6.1         3.0          4.9         1.8  virginica
#> 79           6.4         2.8          5.6         2.1  virginica
#> 80           7.2         3.0          5.8         1.6  virginica
#> 81           7.4         2.8          6.1         1.9  virginica
#> 82           7.9         3.8          6.4         2.0  virginica
#> 83           6.4         2.8          5.6         2.2  virginica
#> 84           6.3         2.8          5.1         1.5  virginica
#> 85           6.1         2.6          5.6         1.4  virginica
#> 86           7.7         3.0          6.1         2.3  virginica
#> 87           6.3         3.4          5.6         2.4  virginica
#> 88           6.4         3.1          5.5         1.8  virginica
#> 89           6.0         3.0          4.8         1.8  virginica
#> 90           6.9         3.1          5.4         2.1  virginica
#> 91           6.7         3.1          5.6         2.4  virginica
#> 92           6.9         3.1          5.1         2.3  virginica
#> 93           5.8         2.7          5.1         1.9  virginica
#> 94           6.8         3.2          5.9         2.3  virginica
#> 95           6.7         3.3          5.7         2.5  virginica
#> 96           6.7         3.0          5.2         2.3  virginica
#> 97           6.3         2.5          5.0         1.9  virginica
#> 98           6.5         3.0          5.2         2.0  virginica
#> 99           6.2         3.4          5.4         2.3  virginica
#> 100          5.9         3.0          5.1         1.8  virginica
drop_obs_if(iris, Sepal.Length < 5 | Sepal.Length >7)
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1            5.1         3.5          1.4         0.2     setosa
#> 2            5.0         3.6          1.4         0.2     setosa
#> 3            5.4         3.9          1.7         0.4     setosa
#> 4            5.0         3.4          1.5         0.2     setosa
#> 5            5.4         3.7          1.5         0.2     setosa
#> 6            5.8         4.0          1.2         0.2     setosa
#> 7            5.7         4.4          1.5         0.4     setosa
#> 8            5.4         3.9          1.3         0.4     setosa
#> 9            5.1         3.5          1.4         0.3     setosa
#> 10           5.7         3.8          1.7         0.3     setosa
#> 11           5.1         3.8          1.5         0.3     setosa
#> 12           5.4         3.4          1.7         0.2     setosa
#> 13           5.1         3.7          1.5         0.4     setosa
#> 14           5.1         3.3          1.7         0.5     setosa
#> 15           5.0         3.0          1.6         0.2     setosa
#> 16           5.0         3.4          1.6         0.4     setosa
#> 17           5.2         3.5          1.5         0.2     setosa
#> 18           5.2         3.4          1.4         0.2     setosa
#> 19           5.4         3.4          1.5         0.4     setosa
#> 20           5.2         4.1          1.5         0.1     setosa
#> 21           5.5         4.2          1.4         0.2     setosa
#> 22           5.0         3.2          1.2         0.2     setosa
#> 23           5.5         3.5          1.3         0.2     setosa
#> 24           5.1         3.4          1.5         0.2     setosa
#> 25           5.0         3.5          1.3         0.3     setosa
#> 26           5.0         3.5          1.6         0.6     setosa
#> 27           5.1         3.8          1.9         0.4     setosa
#> 28           5.1         3.8          1.6         0.2     setosa
#> 29           5.3         3.7          1.5         0.2     setosa
#> 30           5.0         3.3          1.4         0.2     setosa
#> 31           7.0         3.2          4.7         1.4 versicolor
#> 32           6.4         3.2          4.5         1.5 versicolor
#> 33           6.9         3.1          4.9         1.5 versicolor
#> 34           5.5         2.3          4.0         1.3 versicolor
#> 35           6.5         2.8          4.6         1.5 versicolor
#> 36           5.7         2.8          4.5         1.3 versicolor
#> 37           6.3         3.3          4.7         1.6 versicolor
#> 38           6.6         2.9          4.6         1.3 versicolor
#> 39           5.2         2.7          3.9         1.4 versicolor
#> 40           5.0         2.0          3.5         1.0 versicolor
#> 41           5.9         3.0          4.2         1.5 versicolor
#> 42           6.0         2.2          4.0         1.0 versicolor
#> 43           6.1         2.9          4.7         1.4 versicolor
#> 44           5.6         2.9          3.6         1.3 versicolor
#> 45           6.7         3.1          4.4         1.4 versicolor
#> 46           5.6         3.0          4.5         1.5 versicolor
#> 47           5.8         2.7          4.1         1.0 versicolor
#> 48           6.2         2.2          4.5         1.5 versicolor
#> 49           5.6         2.5          3.9         1.1 versicolor
#> 50           5.9         3.2          4.8         1.8 versicolor
#> 51           6.1         2.8          4.0         1.3 versicolor
#> 52           6.3         2.5          4.9         1.5 versicolor
#> 53           6.1         2.8          4.7         1.2 versicolor
#> 54           6.4         2.9          4.3         1.3 versicolor
#> 55           6.6         3.0          4.4         1.4 versicolor
#> 56           6.8         2.8          4.8         1.4 versicolor
#> 57           6.7         3.0          5.0         1.7 versicolor
#> 58           6.0         2.9          4.5         1.5 versicolor
#> 59           5.7         2.6          3.5         1.0 versicolor
#> 60           5.5         2.4          3.8         1.1 versicolor
#> 61           5.5         2.4          3.7         1.0 versicolor
#> 62           5.8         2.7          3.9         1.2 versicolor
#> 63           6.0         2.7          5.1         1.6 versicolor
#> 64           5.4         3.0          4.5         1.5 versicolor
#> 65           6.0         3.4          4.5         1.6 versicolor
#> 66           6.7         3.1          4.7         1.5 versicolor
#> 67           6.3         2.3          4.4         1.3 versicolor
#> 68           5.6         3.0          4.1         1.3 versicolor
#> 69           5.5         2.5          4.0         1.3 versicolor
#> 70           5.5         2.6          4.4         1.2 versicolor
#> 71           6.1         3.0          4.6         1.4 versicolor
#> 72           5.8         2.6          4.0         1.2 versicolor
#> 73           5.0         2.3          3.3         1.0 versicolor
#> 74           5.6         2.7          4.2         1.3 versicolor
#> 75           5.7         3.0          4.2         1.2 versicolor
#> 76           5.7         2.9          4.2         1.3 versicolor
#> 77           6.2         2.9          4.3         1.3 versicolor
#> 78           5.1         2.5          3.0         1.1 versicolor
#> 79           5.7         2.8          4.1         1.3 versicolor
#> 80           6.3         3.3          6.0         2.5  virginica
#> 81           5.8         2.7          5.1         1.9  virginica
#> 82           6.3         2.9          5.6         1.8  virginica
#> 83           6.5         3.0          5.8         2.2  virginica
#> 84           6.7         2.5          5.8         1.8  virginica
#> 85           6.5         3.2          5.1         2.0  virginica
#> 86           6.4         2.7          5.3         1.9  virginica
#> 87           6.8         3.0          5.5         2.1  virginica
#> 88           5.7         2.5          5.0         2.0  virginica
#> 89           5.8         2.8          5.1         2.4  virginica
#> 90           6.4         3.2          5.3         2.3  virginica
#> 91           6.5         3.0          5.5         1.8  virginica
#> 92           6.0         2.2          5.0         1.5  virginica
#> 93           6.9         3.2          5.7         2.3  virginica
#> 94           5.6         2.8          4.9         2.0  virginica
#> 95           6.3         2.7          4.9         1.8  virginica
#> 96           6.7         3.3          5.7         2.1  virginica
#> 97           6.2         2.8          4.8         1.8  virginica
#> 98           6.1         3.0          4.9         1.8  virginica
#> 99           6.4         2.8          5.6         2.1  virginica
#> 100          6.4         2.8          5.6         2.2  virginica
#> 101          6.3         2.8          5.1         1.5  virginica
#> 102          6.1         2.6          5.6         1.4  virginica
#> 103          6.3         3.4          5.6         2.4  virginica
#> 104          6.4         3.1          5.5         1.8  virginica
#> 105          6.0         3.0          4.8         1.8  virginica
#> 106          6.9         3.1          5.4         2.1  virginica
#> 107          6.7         3.1          5.6         2.4  virginica
#> 108          6.9         3.1          5.1         2.3  virginica
#> 109          5.8         2.7          5.1         1.9  virginica
#> 110          6.8         3.2          5.9         2.3  virginica
#> 111          6.7         3.3          5.7         2.5  virginica
#> 112          6.7         3.0          5.2         2.3  virginica
#> 113          6.3         2.5          5.0         1.9  virginica
#> 114          6.5         3.0          5.2         2.0  virginica
#> 115          6.2         3.4          5.4         2.3  virginica
#> 116          5.9         3.0          5.1         1.8  virginica
```
