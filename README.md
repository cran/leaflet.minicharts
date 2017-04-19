[![Travis-CI Build Status](https://travis-ci.org/rte-antares-rpackage/leaflet.minicharts.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/leaflet.minicharts)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rte-antares-rpackage/leaflet.minicharts?branch=master&svg=true)](https://ci.appveyor.com/project/rte-antares-rpackage/leaflet.minicharts)

# Minicharts for Interactive Maps

For a few years now, it has become very to create interactive maps with R thanks to the package `leaflet` by the Rstudio team. Nevertheless, it only provides only a few functions to create basic shapes on a map, so the information that can be represented on a single map is limited: if you have some data associated to some points, you can only represent at most two variables by drawing circles and changing their radius and color according to data.

`leaflet.minicharts` is an R package that provides two functions to add and update small charts on an interactive maps created with the package `leaflet`. These charts can be used to represent as many variables as desired associated to geographical points. Currently, three types of chart are supported: barcharts (the default), pie charts and polar area charts.

Here is an example using this package inside a shiny application:

![](vignettes/minicharts_shiny.gif)

## Installation

For now, the package is only available on Github. You can install it with package `devtools`:

```r
install.packages("rte-antares-rpackage/leaflet.minicharts")
```


## Contributing:

Contributions to the library are welcome and can be submitted in the form of pull requests to this repository.

## License Information:

Copyright 2015-2016 RTE (France)

* RTE: http://www.rte-france.com

This Source Code is subject to the terms of the GNU General Public License, version 2 or any higher version. If a copy of the GPL-v2 was not distributed with this file, You can obtain one at https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html.
