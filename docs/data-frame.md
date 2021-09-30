# Notes on the data-frame object

Inside the application, the track data for a session is loaded into a
data-frame object.  This object (inspired by the R language data frame) allows
efficient access to data.  Originally, data frames were implemented directly
in ActivityLog2, but the implementation was moved into its own separate
package.

The [data-frame
documentation](https://docs.racket-lang.org/data-frame/index.html) is
available in a separate document.  This [blog
post](https://alex-hhh.github.io/2018/08/racket-data-frame.html) provides a
tutorial style introduction to this package.
