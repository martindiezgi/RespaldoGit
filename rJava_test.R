#RESCATAR TABLAS DE PDF#

#Instalar rJava, tabulizer ->
{# https://stackoverflow.com/questions/70036429/having-issues-installing-tabulizer-package-in-r
#install.packages("remotes")
# remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")

#I have just come to dealing with the same problem, but I got to solve it through the following steps:
  
# The tabulizer package requires a Java environment. 
# You need to download Java 64-bit or Java 32-bit through.
# Make sure before that your windows/ mac is 32 bit or 64 bit by using the function sessionInfo().

#Install the rJava package in R.

#Create the Java environment through the command: 
#Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-18/")

#Attention: the path of the file in step 3 is for java 64. the path for java 32 would 
#be Sys.setenv(JAVA_HOME="C:/Program Files (x86)/Java/jdk-18/").
#The other thing is that jdk-18 changes depending on everylaptop, 
#in my case it is jdk-18, in your case it could jdk-17 for example.
#Finally activate the library: library(rJava)
#Voila. rJava and tabulizer work smoothly and nicely.
}

library(dplyr)
library(tabulizer)

#Rescatamos la primera tabla del reporte de la Asociación de Fondos Mutuos
#A Julio 2023
pdf <- "https://www.aafm.cl/2016/wp-content/uploads/2023/08/2023_jul.pdf"
tabla <- extract_tables(pdf, pages=2, output = "data.frame")

df <- tibble(tabla[[1]]) %>%
  janitor::clean_names()

