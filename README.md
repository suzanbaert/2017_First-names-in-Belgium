First names in Belgium
================

Data source
-----------

Belgian government ([data link](http://statbel.fgov.be/nl/modules/publ2ications/statistiques/bevolking/bevolking_-_voornamen_van_de_pasgeborenen_1995-2014.jsp))

Data
----

Data came in the form of two excel files, one for girls and one for boys. Every file contains a sheet for every year, and then numbers per region per name.

Original data file contains the numbers for Belgium as a whole, and for every region seperate (Flander, Wallonia and Brussels). Only the Belgian data is pulled out into the clean file right now.

To note: no names below 5 occurences are present in the database, presumably for privacy reasons.

Cleaning
--------

First part of the code describes a function to get the data in from every sheet into one dataframe, and then merges the boys and girls file.

The clean file is outputted as "First names in Belgium 1995-2016 cleaned.csv"
