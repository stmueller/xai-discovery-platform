# XAI Discovery Platform

The Explainable AI Discovery Platform
======

Shane T. Mueller
Gary Klein
Robert R. Hoffman

## LICENSE

This software is licensed with the GPL 3.0 or later.


## About

This system provides filtering and display functions that let developers or users browse a set of images labeled with a machine learning classifier.  It allows filtering, establishing patterns, and contrasting different conditions to permit discoveries about the AI. In its initial format, it serves as a means for explaining AI through self-explanation and discovery of a user. If heatmap images (e.g., LIME or GRADCAM, etc.) are also available for the image set, these may alternatively be used instead of the original images. With minor changes to the code, both original and heatmap images could be supported.

## Implementation

This is developed with shiny, an interactive web system based  on the R statistical computing language. It must be run using a special server, although it RStudio supports running shiny apps via local computers for testing.


## Implementing the Discovery Platform
To implement the discovery platform, it requires a set of images to display, stored within the images/ subdirectory of the www/ directory. These should be in a format that can be displayed directly in html (jpg, gif, or png).  Three additional files are used for setting up discovery platform. The basic directory/file structure is:
<pre>
app.R        Main shiny code
main.csv     Main data table
probs.csv    Probability table
info.json    Customize the discovery platform
www/         web files
www/fns.js   Custom javascript
www/images   images of each case to be browsed.
</pre>

* main.csv
This file contains information about the entire set of images in comma-separated format. Columns must be labeled as in the following example:

<pre>
case,	classID,	class,	labelID,	label,	corr,	fname
3,	4,	4,	4,	4,	TRUE,	./images/D0/img00000003.png
4,	1,	1,	1,	1,	TRUE,	./images/D0/img00000004.png
7,	1,	1,	1,	1,	TRUE,	./images/D0/img00000007.png
12,	5,	5,	1,	1,	FALSE,	./images/D0/img00000012.png
19,	6,	6,	6,	6,	TRUE,	./images/D0/img00000019.png
</pre>

Case is a unique but arbitrary numeric label for each image. classID is a numeric identifier (1 and above) for the class of the image. class is a text (or number) naming that class. Similarly, labelID and label describe the best label given to the image by the classifier. corr determines whether it is judged as correct (could be coded 0/1 or TRUE/FALSE), and fname indicates the filename where the image representing the case is stored (within the www/ directory.)


* probs.csv
This file contains the 10 top labels given to each case by the classifier, with probabilities or strengths describing the relative strength of each alternative.  This file must be exactly 20 columns. The first row must be labels, but the labels can be anything and are not used directly.  The file must have the same number of rows as main.csv.

<pre>
"X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X1.1","X2.1","X3.1","X4.1","X5.1","X6.1","X7.1","X8.1","X9.1","X0"
1,2,3,4,5,6,7,8,9,10,4e-04,0.0239,0.0174,0.9179,0.0097,0.0037,0.0069,0.0013,0.0167,0.0022
1,2,3,4,5,6,7,8,9,10,0.976,0.0093,0.0013,3e-04,0.0014,3e-04,0.0012,0.0096,4e-04,1e-04
1,2,3,4,5,6,7,8,9,10,0.9443,0.0101,0.0181,6e-04,0.005,0.0028,0.0033,0.0112,0.0045,2e-04
1,2,3,4,5,6,7,8,9,10,0.38,0.2249,0.0093,0.0238,0.2596,0.0169,0.0221,0.05,0.0071,0.0065
1,2,3,4,5,6,7,8,9,10,0.0047,0.0153,0.0308,0.0129,0.2841,0.6082,0.0038,0.0201,0.0087,0.0113
1,2,3,4,5,6,7,8,9,10,0.0283,0.2543,0.1173,0.0038,0.2903,0.1773,0.0469,0.0275,0.0399,0.0145
1,2,3,4,5,6,7,8,9,10,0.0733,0.0319,0.026,0.0072,0.829,0.0023,0.0161,0.0065,0.006,0.0017
1,2,3,4,5,6,7,8,9,10,0,0.0019,2e-04,7e-04,5e-04,0.9942,1e-04,6e-04,6e-04,0.0012
1,2,3,4,5,6,7,8,9,10,0.0042,0.0015,0.2508,0.0013,0.675,0.0011,0.0254,0.0236,0.0152,0.0019
</pre>

The first ten columns indicate the top label responses given for a case, in any order.  The second ten indicate probabilities associated with each label. The order of labels can be different for each row, but the probabilities and the labels must be in the same order for each row of the data.  For cases in which fewer than 10 labels are returned by a classifier, use NA to fill the rest of the first ten label columns. The second ten columns can be 0 or NA if not used. If more than ten labels are returned by the classifier,only the top 10 should be entered in a row.  

The labels in this file must start with 1, and correspond to the numbers in labelID column of main.csv.


* info.json
This is a standard .json file that contains some of the branding/information to allow customizing the discovery platform without editing the .R code.

<pre>
{
    "title":"XAI Discovery Platform | MNIST Sample Data",
    "subtitle":"Michigan Technological University | MacroCognition | IHMC",
    "contact":"<a href='http://shanetmueller.info'>Shane T. Mueller</a> (shanem@mtu.edu)",
    "about":"About: MNIST data with linear SVM classifier trained on 10,000 cases. Classifier achieved 89.2% accuracy on training set, and a similar accuracy (89.2%) was achieved for set of 50,000 validation cases. This platform browses 10,000 validation cases, sampled randomly so that 5,000 were correct and 5,000 were errors",
    "otherFeatures":[],
    "classNames":["1","2","3","4","5","6","7","8","9","0"],
    "labelNames":["1","2","3","4","5","6","7","8","9","0"]
}
</pre>

Along with titles and contact, there are three additional optional settings. classNames and labelNames specify the order that the levels of class and label should be displayed in, and must contain all levels found in main.csv of each of these columns. If these are not provided, the default order will be used, which is likely to be alphabetical.  Next, otherfeatures indicates any additional feature columns (to be included in main.csv) that can be used for filtering and sorting. This might be a set of higher-level categories or classifications that would be useful to compare on. Support for using these is fairly limited.

## Demonstration site
The code includes a .R file called a build-discovery-platform.R, which will create a simple SVM classifier on a subset of the MNIST handwritten letter digits. I recommend you use RStudio to run this, which is downloaded and installed separately from the main R software.

To build the demonstration site, you must download the MNIST data as a .csv file from Kaggle at https://www.kaggle.com/oddrationale/mnist-in-csv. Only the file mnist_train.csv is used, and it must be unzipped/saved directly in the archive/ subdirectory (the zip file has its files in an archive directory already.)  This code requires several libraries to use.  It will load this data, create a svm classifier based on 10,000 cases (of the 60,000 total), and then classify both the training and remaining 50,000 cases, on which it achieves an accuracy of about 90%. Training this model and classifying the rest of the cases can take 30 minutes or more depending on your computer.  Then, a sample of 10,000 cases (5,000 correct and 5,000 error) are used for the discovery platform, and the properly-formatted main.csv and probs.csv files are generated.  Finally, at the end (if this is enabled), each case will be saved as a .png image within www/images, spread across subdirectories with at max 1000 files per directory.

Once this file run is completed successfully, you should be able to run app.R directly in RStudio with the 'Run App' button at the top of the editing pane. This will run the site within RStudio, but you can open in a web browser window by clicking on the 'Show in new window' button in the R Studio Viewer.  If you want others to access this server, you must run from a shiny server, which is possible via appropriate software installation on a linux server.  Several companies offer shiny app hosting for affordable rates.


## Limitations/Potential future features


* Database size.  Because the 'selected case' pulldown is generated from the data, this pulldown is limited to the first 1000 cases. Creating that pulldown appears to be the main computationally-intensive operation, at least for our test cases with a limited number of labels (10).  For systems with 100s or 1000s of categories, some of the bargraph visualizations might break down, and these may need to be adapted or removed to allow the system to work.

* Input format.  Currently, the data is read in via a specially-formatted .csv file.  R and shiny offer capabilities to read directly from databases, but this is not supported in the current system.  R reads in the entire data table once when the page is loaded, and will not read again unless the page is reloaded, so the server-side burden is fairly limited.  Extremely large databases could limit system performance, but the MNIST system with 10,000 cases provides a reasonable size to explore, especially when errors are over-sampled.

* Other visualizations.  Currently, only a single visualization is supported, and it should be thumbnailable in a 100x100 window. Currently, clicking on the image opens a pop-up window, and this could support multiple views (heatmap overlays), but that capability would have to be added, perhaps with additional columns in main.csv.  Something like LIME, GRADCAM, or the like could be incorporated fairly easily in that fashion.

* Selecting similar examples. Currently, the 'most similar' cases are selected based on Kullback-Leibler distance between cases in the confusion space.  Models may permit identifying a similarity space and neighbors directly, but this type of similarity is not currently used.  Furthermore, this kind of similarity may require substantial realtime computation to work appropriately.  

* Incorporating additional features. Many times, the input database may have been coded with additional features other than just the ground truth category and the AI classification label. This is only supported via filtering view, but might be incorporated in the categories and contrasts views more directly.

* Non-ground-truth cases. Currently, the system assumes you have a labeled set of images where each ground truth is a single objective category. Sometimes, you may have cases different levels of agreement from multiple human coders or with multiple true labels (40% said woman, 40% said lady, 20% said human). The system currently does not incorporate multiple true labels or display different levels of agreement, but this could be added as additional columns in main.csv and this might be exposed via the UI.


* Classification hierarchies. Many classifiers provide a hierarchy of labels (product|tool|hammer|ball-peen hammer) with different strengths of response. Currently, these can be handled as alternative labels, or by pre-processing with ontologies to exclude alternatives that are hyponyms or hypernyms.  Correctness is coded in the data file, so it is possible to score a classification is correct even if its best-matching label is NOT (e.g., calling a hammer a 'product'). However, the UI may not make this obvious.


* Video. It may be possible to encode video thumbnails as animated .gif files, although this could be distracting.  An alternative is to make video available via another filename specified in a second column of main.csv, and provide javascript to open a control window to view this.

* Non-imagery (text, audio, other).  This is likely to not work well, although special-purpose web interfaces could be developed for some of these, provided a thumbnail of the asset can be created and used in the discovery platform.

* URL parameters.  Shiny supports processing with web page url parameters, but the current platform avoids. We have experimented with this, and it is possible to incorporate, but tricky to get right. This would permit linking to specific cases by a direct URL.
