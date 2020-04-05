COVID-19 mitigation with positive deviance
====

This apps helps identify counties to use as comparative deviants based on the size and population desnity of your county.

To access online, R is not needed on your computer, browse to https://public.opencpu.org/ocpu/github/qitools/positivedeviance/www/

To install in R on your computer:

    library(devtools)
    install_github("opencpu", "jeroenooms")
    install_github("COVID-19-mitigation-with-positive-deviance", "qitools")

    library(opencpu)
    opencpu$browse("library/COVID-19-mitigation-with-positive-deviance/www")

Use the same function using a local webserver on your computer:

    library(COVID-19-mitigation-with-positive-deviance)
    COVID-19-mitigation-with-positive-deviance()
    ?COVID-19-mitigation-with-positive-deviance

For more information about OpenCPU apps, see [opencpu.js](https://github.com/jeroenooms/opencpu.js#readme)
