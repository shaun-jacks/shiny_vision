
# shiny_vision

####  R Shiny Implementation with Google Vision APIs.

This project is meant to explore the use of R Shiny with cloud APIs such as Google Vision.

There are two motivations:

1. Provide other R users a template for integrating Google Vision with their R Shiny applications.
2. Apply Google Vision in an R shiny project for a health application such as:
	*  emotion recognition for those who have trouble with social interactions, validation of coded image data, and many other applications.
3. The following features from Google Vision has been implemented with this application:
	* **Facial Detection**, **Object Localization**, **Text Detection**, **Landmark Detection**, and **Logo Detection**.


**A project demo can be found [here](https://shaun-jacks.shinyapps.io/visionanalyze/).** Note that the demo works most reliably on a desktop computer as opposed to mobile devices.


### Prerequisites
First, be sure to have `R` installed on your computer. The version this application was developed with is `R version 3.5.1`. Here's a link to [get started](https://www.r-project.org/). In addition, [R Studio](https://www.rstudio.com/) may also be needed.

Once you have R installed on your computer, run
`install.packages("devtools")` in your R session.

Next, be sure to install `RoogleVision` and `magick` libraries. 

This particular application uses a forked version of `RoogleVision` in order to use additional features such as **object localization** not yet in the original. 
* It can be installed with `devtools::install_github("shaun-jacks/RoogleVision")`.

* Run `install.packages("magick")` to install the magick library

Once you have the prerequisite libraries, run `devtools::install_github("shaun-jacks/shiny_vision")` to download repo, or fork this repository.

### Getting Started

This project requires Google Authentication. To authenticate, the following [link](https://github.com/cloudyr/RoogleVision/blob/master/README.md) can help you get started. Follow the instructions, and then **copy your client_id and client_secret to your .Renviron**.

When you have your client_id and client_secret keys, save these to your .Renviron with  `file.edit("path/to/.Renviron")`.  In my case, `file.edit("~/.Renviron")`. 

Now, add the following lines:
* `googleAuthR.cliend_id = "YOUR_CLIENT_ID_HERE"`.
* `googleAuthR.client_secret = "YOUR_CLIENT_SECRET_HERE"`.

Now save the file and Restart your R session.

### Use

To run the app, `runApp("visionAnalyze")`.

## Authors
* **Shaun Jackson**

## License

This project is licensed under the MIT License, for further details, see [LICENSE](https://github.com/shaun-jacks/shiny_vision/blob/master/LICENSE).

## Acknowledgments

Thanks to [RoogleVision](https://github.com/cloudyr/RoogleVision), and the [Google Vision](https://cloud.google.com/vision/) team for making this project possible using the APIs created. Also thanks to [HackDavis19](https://hackdavis.io/), which was a vital catalyst to getting this project started when thinking of possible Machine Learning Health Applications. 

Lastly, this [blog](https://github.com/stoltzmaniac/ML-Image-Processing-R/blob/master/Google%20Vision%20API/Google%20Vision%20API%20in%20R.md) by Scott Scoltzman was extremely helpful for getting started, as well as this [TensorFlow for R blog](https://blogs.rstudio.com/tensorflow/posts/2018-11-05-naming-locating-objects/). In addition, this [magick intro](https://cran.r-project.org/web/packages/magick/vignettes/intro.html) also provided much aide.
