## Notices:
## This software may be used, reproduced, and provided to others only as permitted
## under the terms of the agreement under which it was acquired from the U.S.
## Government. Neither title to, nor ownership of, the software is hereby
## transferred. This notice shall remain on all copies of the software.
 
## Copyright 2016 United States Government as represented by the Administrator of the
## National Aeronautics and Space Administration. All Rights Reserved.
 
## Disclaimers
## No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY 
## KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY
## WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED 
## WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM 
## INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY 
## WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. 
## THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT 
## AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, 
## SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT 
## SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES 
## REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND 
## DISTRIBUTES IT "AS IS." 
 
## Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE 
## UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR
## RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, 
## DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES 
## FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT 
## SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES 
## GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO
## THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE
## THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


## PiCo: Pixelwise Correlation-Based Landscape Classification

## PiCo was developed for simplified running of CLaRe (Climate Landscape Response) Metrics developed by Cynthia Wallace et al. 2016
## It can perform pixelwise simple or multilinear regressions to uncover landscape dynamics between a chosen variables
## The sample code here provides for correlations between NDVI, precipitation and evapotranspiration datasets
## Any variables can be studied as long as there are the same number of rasters for each dataset and all rasters are at the same extent

## Inputs
## Folder paths to where individual datasets are stored
## In this sample code, there are three file paths used to create stacks of MODIS NDVI rasters, 
## PRISM Precipitation rasters, and MODIS ET rasters

## Outputs
## One raster with pixelwise coefficient of determination, or r-squared values.
## The code also allows for the output of other regression values at the pixel level, see lines 115-119


##########################################################################################################################
########################################################################################################################
## Install required packages
## Raster Package
install.packages("raster")

## Rgdal Package
install.packages("rgdal")

## Load in Required Libraries
library('raster')
library('rgdal')

#################################################################################################################################
## Output is a raster with same extent as the input rasters
## Load one input raster(complete file path and file name with extension) into the "" in line 24

information_for_new_raster <- raster("C:\\Users\\spater\\Desktop\\COOL_SEASON\\Class_6\\PRISM\\2010\\Day_24\\precip035.tif_24d_2009.tif.tif")
information_for_new_raster

## The spatial information will be copied from the information_for_new_raster to generate the blank raster in which correlation values
## will be added

Proper_ncell<- ncell(information_for_new_raster)
Proper_nrows<- nrow(information_for_new_raster)
Proper_ncols<- ncol(information_for_new_raster)
Proper_xmin<- xmin(information_for_new_raster)
Proper_xmax<- xmax(information_for_new_raster)
Proper_ymin<- ymin(information_for_new_raster)
Proper_ymax<- ymax(information_for_new_raster)
Proper_crs<-projection(information_for_new_raster)

## Select name with extension for final output raster:
newName = "C6_2012_32Day.tif"

#############################################################################################################################
## Beginning PiCo

## Each line of the stacking sections must be run INDIVIDUALLY to ensure working directory is changed and the correct rasters
## are stacked. The first stack will be the response variable.

######################################################## Response Variable#############################################################
## Stack Response Variable Rasters
##In this example: MODIS NDVI rasters

setwd("C:\\Users\\spater\\Desktop\\COOL_SEASON\\Class_6\\NDVI\\2012")
files_modis <- list.files(getwd(), pattern="\\.tif$")
stackMODIS<- stack(files_modis)

#####################################################################################################################################

## The next section will stack the Explanatory Variables
##For CLaRe, these include PRISM Precipitation Rasters and ET. More explantatory variables can easily be added by following the
## same setwd, list.files, stack() format. These new variables must be added to both the getValues, na.omit, and function sections

######################################################## Explanatory Variables#######################################################
## Stack Explanatory Variable Rasters
###In this example: PRISM rasters
setwd("C:\\Users\\spater\\Desktop\\COOL_SEASON\\Class_6\\PRISM\\2012\\Day_32")
files_PRISM <- list.files(getwd(), pattern="\\.tif$")
stackPRISM <- stack(files_PRISM)

#####################################################################################################################################
## Stack Explanatory Variable Rasters
###In this example: ET rasters
setwd("F:\\MODIS09_ET")
files_ET<- list.files(getwd(), pattern="\\.tif$")
stackET<- stack(files_ET)

####################################################################################################################################

## Retreive values of all stacks, three in this example
## Stacks are converted to matrices, the columns are individual rasters and the rows are pixel values
## Pixels are read from the raster right to left

MODIS <-getValues(stackMODIS)
PRISM<- getValues(stackPRISM)
ET<- getValues(stackET)

######################################################################################################################################
## Replace NA values with 0
## If using MODIS ET data, from University of Montana, NA Value is 180

MODIS[is.na(MODIS)]=0
PRISM[is.na(PRISM)]= 0
ET [is.na(ET)] =0

## If using University of Montana data:
ET[ET==180] <- 0

############################################################################################################################
## Simple Linear and Multi-Linear Regression Codes
## Choose one out of the following
## If any more explanatory variable stacks were added --> add variable name to f1
## For example, if temperature data was added with variable name "temp" the function should read:
## f1 <- function(n){summary(lm(MODIS[n,]~ PRISM[n,] + ET[n,], + temp[n,]))$r.squared}

## If any other value besides $r.squared is desired, 
##slope = $coefficients[2]
##intercept = $coefficients[1]
## $fstatistic
## see ?summary.lm for more summary outputs

## Run either f1 or f2

## Multi-Linear
f1 <- function(n){summary(lm(MODIS[n,]~ PRISM[n,] + ET[n,]))$r.squared}

## Simple-Linear
f2 <- function(n){summary(lm(MODIS[n,]~ PRISM[n,]))$r.squared}

## Set to either f1 or f2
chosen_function<- f2


#########################################################################################################################

## Create an empty numeric vector
## This vector will be populated by results of the fuction chosen

Correlation_Values <-vector("numeric", Proper_ncell)

######################################################################################################################
## Run line 145 by itself
## Highlight and run 146 to 150 together

## This loop goes through all pixels, calculates the function chosen, and adds the output to Correlation_Values

x<-1
while(x <Proper_ncell){
  Correlation_Values[[x]] <- chosen_function(x)
  x <- x+1
}

######################################################################################################
## Convert the vector list Correlation_Values to the matrix Correlation_Values_Matrix

Correlation_Values_Matrix<- as.matrix(Correlation_Values)

#######################################################################################################################

## Generate New Raster with pixel-wise correlation values

## Set new working directory to where the output raster should be saved
setwd("C:\\Users\\spater\\Desktop\\COOL_SEASON\\Class_6\\Results")

## Creates an empty raster with desired extent and coordinate reference system
newRaster<- raster(nrows= Proper_nrows, ncols= Proper_ncols, xmn= Proper_xmin, xmx= Proper_xmax, ymn= Proper_ymin, ymx= Proper_ymax, crs= Proper_crs)

##Assign Values from Correlation_Values_Matrix to empty raster
values(newRaster) <-Correlation_Values_Matrix
cdd<- as.vector(Correlation_Values_Matrix)
setValues(newRaster, Correlation_Values_Matrix)

## Replace 0 values with NA
newRaster[newRaster==0]<- NA

## Write the new raster with desired name and output location
writeRaster(newRaster, newName, format="GTiff", overwrite=TRUE)


## Clean up Global Environment before running again -- IF USING SAME RESPONSE RASTERS use second option
## Remove all variables and values from Global Environment
rm(list = ls())

##Remove just a selection
rm(files_PRISM, stackPRISM, PRISM, files_ET, stackET, x, Correlation_Values, Correlation_Values_Matrix, newRaster, newName,
  cdd)

##########################################################################################################################
##########################################################################################################################