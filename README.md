# SheFireModel

Soil Heating in Fire (SheFire) Model:  Additional detailed files and an R package to build and use a SheFire model for how different soil depths heat and cool during fires

The model was developed by Mary K Brady in collaboration with Matthew Dickinson, Jessica Miesel, and Erin J Hanan

The SheFire folder contains an R package that can be installed directly from github (if you have devtools installed) using the code: install_github("FireLab/SheFireModel/SheFire")

The AnnotatedScripts folder contains .Rmd files that go through the model construction step by step with detailed comments and explanation as well as a small sample data set. There may be minor variations between the newest model version and the .Rmd scripts but they should be close enough for learning how the model works


If you wish to contribute to this model, please do! But also please follow the best practices outlined below:

If you contribute or improve an application function:

     make sure it works with the WlkrPlot4NE.csv example data set
     put your name and contact info on the new/updated function help/documentation page
     make the help page actually helpful for folks
     function names are capitalized, parameters are not
     parameter names should be consistent with other functions in the package (for example: x is always a depth, t is always time)
     update the model version number by 0.01 (if it was 1.02, now it is 1.03)
     
If you contribute to the model development (SheFire function):

     make sure it still works with WlkrPlot4NE.csv example data set
     add your name to the authors list in the package description 
     adjust the help/documentation page as needed
     if you add parameters, set default values such that function calls people wrote in any scripts with the earlier model version will still work
     model time starts at 0.00001 minutes
     please! consider updating the annotated .Rmd scripts to reflect your changes
     update model version number by 1 for major changes, or 0.1 for any non-major changes (if it is a minor enough change that you would consider 0.01, is it worth changing?)
