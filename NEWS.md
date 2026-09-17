# rankinPlot 1.2

Changes have been made to grottaBar()
* A new colour scheme has been added, "whiteBlueGradient" which is now the default option.
* Grottabar now accepts an arbitrary number of colours for the printed numbers. The old approach (the textCut argument) is depreciated.
* Use of a "custom" colour scale is depreciated. Instead, a ggplot2 scale_fill_ function can now be fed in as an argument to colorScheme.

In addition, a new function pp_plot() has been added that creates probability-probability plots.

# rankinPlot 1.1

grottaBar() function has several new options:

* Printing of count (percent) format for information
* The ability to specify a font face (plain, bold, italic) for the numbers printed in the Grotta Bar.
* The ability to specify two colours for the numbers printed in the Grotta Bar.

Examples of these options may be found in the documentation for the grottaBar function.

Several other quality of life changes have been added:

* The package now supports both spellings of "colour".
* printNumbers options may now be abbreviated


# rankinPlot 1.0

Package released
