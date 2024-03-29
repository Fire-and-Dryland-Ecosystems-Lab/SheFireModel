# SheFireModel
Soil Heating in Fire (SheFire) Model: An R package and detailed explanatory files to build and use a SheFire model for how soil heats and cools during fires

The model was developed by Mary K Brady in collaboration with Matthew B Dickinson, Jessica Miesel, and Erin J Hanan

The SheFire folder contains an R package that can be installed directly from github (if you have devtools installed) using the code: devtools::install_github("Fire-and-Dryland-Ecosystems-Lab/SheFireModel/SheFire")

The AnnotatedScripts folder contains .Rmd files that go through the model construction step by step with detailed comments and explanation as well as a small sample data set. There may be minor variations between the newest model version and the .Rmd scripts but they should be close enough for learning how the model works


If you wish to contribute to this model, please do! But also please follow the best practices outlined below:

If you contribute or improve an application function:

     make sure it works with the WlkrPlot4NE.csv example data set
     put your name and contact info on the new/updated function help/documentation page
     make the help page actually helpful for folks
     function names are lowercase and use "_" between words, parameters are lowercase too and use "." between words
     parameter names should be consistent with other functions in the package (for example: x is always depth, t is always time)
     
If you contribute to the model development (shefire function):

     make sure it still works with WlkrPlot4NE.csv example data set
     adjust the help/documentation page as needed
     if you add parameters, set default values such that function calls people wrote in any scripts with the earlier model version will still work
     keep in mind: model time starts at 0.00001 minutes but equation fitting starts at 0
     please! consider updating the annotated .Rmd scripts to reflect your changes

To have your contribution added to the model, create a pull request to the "development" branch and inculde a summary of what you added or changed as well as the relevant results using the WlkrPlot4NE.csv data set (either the function output or the model object from SheFire). We will periodically update "main" with the changes pulled to "development"

https://doi.org/10.5281/zenodo.4694828
