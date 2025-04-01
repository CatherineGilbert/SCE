# Seasonal Characterization Tool
This project uses APSIM and the R apsimx package to produce seasonal covariates for maize or soy trials.
#TODO: Add a section on motivation/what problem does this project solve?

## Running the tool

### Correctly formatting the input file:
1. First, you will need a `.csv` file with columns `Site`, `Latitude`, `Longitude`, `Genetics`, and `PlantingDate`.
   1. `Site`: human-readable identifier for the trial location. 
   2. `Latitude`/`Longitude`: Standard numeric WGS84 coordinates.
   3. `Genetics`: Cultivar maturity value. 
      1. For Soybean, `Genetics` is a numeric value from `-2:10`. There are also early (`.00 - .33`), standard (`.33 - .66`), and late (`.67 - .99`) variants of each of these maturity values; denoted by the decimal value.
      2. For Example, a Genetics value of `3.8` indicates the cultivar is a late Maturity III, `0.2` would be an early Maturity 0, and `-1.5` would be a standard Maturity 000.
      3. For Maize, `Genetics` is a character string with the approximate number of days to maturity, and a letter. `A` for early, and `B` for late maturing varieties.
      4. The posssible maturity values are `80, 90, 95, 100, 103, 105, 108, 110, 112, 115, 120, and 130`. The input will be matched to the closest of these values.
      5. Valid input formats for maize genetics include: `A_100`, `A100`, `100a`.
   4. `PlantingDate`: the date the trial is sown in `YYYY-MM-DD` format or just `YYYY`. If only the year is provided, the planting date will default to the first suitable day of that year. You can modify this default behavior in the `Sowing` module.
2. Once your `.csv` file is prepared, go to lines 17 and 18 of `apsimx.R`. Set `codes_dir` to the full file path of the parent folder that contains `apsimx.R`, and then you may modify the output directory path on line 18.

> See the example inputs in `./example_input_files` for formatting examples or testing purposes.

### Running the script outside the web app:
1. Download and extract the project, or clone from github onto your local computer.
2. Ensure you have R [installed and set up](https://rstudio-education.github.io/hopr/starting.html)
3. Make sure you have the following library dependencies:
   1. ???
4. Run(#TODO: Knit?)  `apsimx.R` using RStudio or your R interpreter of choice.

### Interpreting the output:
#TODO


## Collaborators:

#TODO: Have Sam and Catherine update their info and provide whatever links (github/social media) they want. Come up with a decent format, too.
* Catherine Gilbert
* Sam Shi
