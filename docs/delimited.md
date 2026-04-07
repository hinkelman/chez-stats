# Read and Write Delimited Text Files

There is nothing sophisticated about this approach to reading delimited text files. For all files, `read-delimited` produces a list of lists of strings.
There is no attempt to convert strings to numbers or other objects. The file contents needs to be rectangular, i.e., every row must have the
same number of columns. For an alternative approach, see [chez-csv](https://akkuscm.org/packages/chez-csv/).

## `(read-delim path [sep-char max-rows])`

**returns:** a list of lists where each sub-list is one row in the file at `path`; `sep-char` and `max-rows` are optional and default to `#\,` and `+inf.0`, respectively

## `(write-delim lst path [sep-char overwrite])`

**writes:** a list of lists `lst` as a delimited text file to `path`; `sep-char` and `overwrite` are optional and default to `#\,` and `#t`, respectively.

## Example

```scheme
> (define example-list (list
                        (list "col1" "col2" "col3" "col4")
                        (list 10.02 #\A "1,000" "Glen \"Big Baby\" Davis")
                        (list 1/3 #\B "1000" "Earvin \"Magic\" Johnson")))

> (display example-list)
((col1 col2 col3 col4) (10.02 A 1,000 Glen "Big Baby" Davis) (1/3 B 1000 Earvin "Magic" Johnson))

> (write-delim example-list "example.csv")

> (read-delim "example.csv")
(("col1" "col2" "col3" "col4")
  ("10.02" "A" "\"1,000\"" "\"Glen \"Big Baby\" Davis\"")
  ("0.3333333333333333" "B" "1000" "\"Earvin \"Magic\" Johnson\""))
```
