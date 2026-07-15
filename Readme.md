Uses APSIM + the R apsimx package to produce seasonal covariates for simulated trial conditions. 

The full documentation is [available in the project files](https://github.com/CatherineGilbert/SCE/blob/main/www/SCE_Documentation.html). The first page of the app provides a button to view this documentation in-browser. 

### Running the App

#### Directly: 
To run the app, you'll need an installation of R (available from https://www.r-project.org/) and a copy of Next-Gen APSIM (available from https://www.apsim.info/download-apsim/) available on your system. Download the GitHub repository and open "app.R" and "apsimx.R" in the IDE of your choice. You will likely be prompted to download missing packages. Do so. Then launch "app.R".

#### As a Docker container:
For the sake of reproducibility, the SCE app is bundled with a Dockerfile and the files necessary to build the working environment. “renv.lock” contains the versioned R packages; the folder “next_gen_apsim” contains the files for the Next-Gen APSIM distribution. In the case that packages and software become outdated, building from the Dockerfile allows you to replicate the app in its original environment. 

  To run the app as a Docker container:  
  1. (If you don't have it already) Install Docker. https://docs.docker.com/get-started/get-docker/  
  2. Download this GitHub directory.   
  3. Through the terminal, navigate to the downloaded directory and run  
  ```
  docker build -t sce-app .  
  ```  
  4. When the image has been built, run   
  ```
  docker run -p 3838:3838 sce-app  
  ```  
  5. Open a browser. The application should be available at http://localhost:3838/.  


### Quick Start 

User control of the app takes place on the "Upload and Analyze" tab. 

To run the seasonal characterization, you'll need to provide an input file with the starting parameters of each trial. This will be a .csv with the columns "Site", "Latitude", "Longitude", "Genetics," and "Planting". Each row of the file represents a set of conditions to characterize. 

When creating an input file:
* "Site" is the name of the location. 
* "Latitude" and "Longitude" are standard numeric WGS84 coordinates.   
* "Genetics" refers to the cultivar maturity genetics. This column is formatted differently depending on the crop maturity system, which is specified with the "Maturity Handling" dropdown.
  * For soybean maturity handling, Genetics contains numeric values in the range -2 -- 10, corresponding to soybean maturity groups 000 -- X. Adding 0 -- 0.33 to the Genetics value classifies the maturity as "early" within a maturity group, between 0.34 -- 0.66 classifies it as standard ("mid"), and between 0.67 -- 0.99 classifies it as "late." For example, a Genetics value of 3.8 would be a late maturity III, 0.2 would be an early maturity 0, and -1.5 would be a standard maturity 000.   
  * For maize maturity handling, the Genetics column contains character strings with the approximate number of days to maturity for the cultivar and a letter, A or B, for early or late maturing varieties. This follows the standard maize RM naming scheme. Formatting is flexible (ex: "A_100", "A100", "100a" are all valid). Input values are matched to the closest available APSIM cultivar. 
* "Planting" is when the crop is sown. This can be provided as a date in YYYY-MM-DD format, or as only the year. If no planting date is specified, the simulation will sow the trial on the first suitable day of the year.

For a first analysis, you can choose to use an example input file instead. Under "Upload Input File", check "Use example file?" and then "Maize" or "Soybean". These files can be downloaded for viewing / reference via the front page of the app. 

Running the seasonal characterization requires a template .apsimx crop simulation for generating the trial simulations. You can similarly check "Use example file?" under "Select Template Crop Model" and then "Maize" or "Soybean" to use the respective template model. 

Choose the maturity handling option that matches your crop of choice. Then hit "Run Analysis" to preform the seasonal characterization. When the characterization is finished, the rest of the app's interface will become available, allowing you to download the results of the seasonal characterization or perform further analysis. 

See the [provided documentation]( https://github.com/CatherineGilbert/SCE/blob/main/www/SCE_Documentation.html) for more information on user control.
