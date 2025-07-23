Uses APSIM + the R apsimx package to produce seasonal covariates for simulated trial conditions. 

The full documentation is [available in the project files](  https://github.com/CatherineGilbert/SCE/blob/main/SCE_Documentation.docx).

### Running the App

#### Directly: 
To run the app, you'll need an installation of R (available from https://www.r-project.org/) and a copy of Next-Gen APSIM (available from https://www.apsim.info/download-apsim/) available on your system. Download the GitHub repository and open "app.R" and "apsimx.R" in the IDE of your choice. You will likely be prompted to download missing packages. Do so. Then launch "app.R".

#### As a Docker container:
For the sake of reproducibility, the SCE app is bundled with a Dockerfile and the files necessary to build the working environment. “renv.lock” contains the versioned R packages; the folder “next_gen_apsim” contains the files for the Next-Gen APSIM distribution. In the case that packages and software become outdated, building from the Dockerfile allows you to replicate the app in its original environment. 

  To run the app as a Docker container:  
  1. (If you don't have it already) Install Docker. https://docs.docker.com/get-started/get-docker/  
  2. Download the GitHub directory.   
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

To run the seasonal characterization, you'll need to provide a custom file describing the trial conditions. This file should be a .csv with the columns "Site", "Latitude", "Longitude", "Genetics," and "Planting". Each row of the file represents a set of trial conditions to characterize. 

* "Site" is the name identifier for the location. 
* "Latitude" and "Longitude" are standard numeric WGS84 coordinates.   
* "Genetics" refers to the cultivar maturity.   
  * For soybean, Genetics is a numeric in the range -2 -- 10 (corresponding to maturity groups 000 -- X). There are early, standard, and late variants of each of these cultivar maturities. Adding 0 -- 0.33 to the Genetics value will classify the maturity as "early" within a maturity group, between 0.34 -- 0.66 will classify it as standard ("mid"), and between 0.67 -- 0.99 will classify it as "late." For example, an input Genetics value of 3.8 would be a late maturity III, 0.2 would be an early maturity 0, and -1.5 would be a standard maturity 000.   
  * For maize, Genetics is a character string with the approximate number of days to maturity for the cultivar and a letter, A or B, for early or late maturing varieties respectively. The cultivar maturities available are 80, 90, 95, 100, 103, 105, 108, 110, 112, 115, 120, and 130. The input will be matched to the closest of these values and the early or late variant. Several input formats (ex: "A_100", "A100", "100a") are acceptable.   
* "Planting" is when the trial is sown. This can be provided as a date in YYYY-MM-DD format, or as only the year. If no planting date is specified, the simulation will sow the trial on the first suitable day of the year.

Example input trial files are available in the "example_input_files" folder. 

Running the seasonal characterization also requires a template .apsimx crop model to use to generate the trial simulations. To run characterizations for maize or soybean, you can select the "Maize_Template.apsimx" or "Soy_Template.apsimx" files provided in the "template_models" folder. Choose the maturity handling option that matches your crop of choice. Then hit "Run Analysis".

See the [provided documentation]( https://github.com/CatherineGilbert/SCE/blob/main/SCE_Documentation.docx) for more information on user control.
