Mímisbrunnr

This document explain the folder structure and descrive each of the files that
you can find in the project main folder.


/data

    This folder save the data in the project. Notice that we have real data and
    fake data for testing porpuses. The real data contain 1000ish rows with a
    frienship network averaging 3 friends per person. So is not practical to
    do testing with that. In order to test thing faster, we have the fake data.


    /aureus

         Everything related to the S.Aureus data.


         /csv

             In here are store the files that the R scripts actually use and
             load into memory.

             "saureus_19022020.csv"

                 The data that contain the S.Aureus after converting into CSV.
                 Notice that small changes from the original data might be
                 present, as for example, changing "," for "." so whatever
                 doesn't go wonkers trying to understand decimal points, strings
                 NULL, NAs, "NA", or other technical problems.


         /doc

              Documentation that explain what we have in the data.

              "metadata_nasal_throat_swabs_FF1_20022020.xlsx"

                  Explanation of each column about the S.Aureus.


         /original

              Verbatim copy of the recieved data, totally unmodified since it
              was downloaded. Including the file name.

              "hash.sha1"

                  SHA1 (160-bit) checksums for each file cointain in this folder

                  This is useful to check that everybody has the same file with
                  the same data quickly.

              "PERSKEY_Rafael_s aureus_19022020.xls"

                  The original S.Aureus data, it also contain the frienship
                  network and some of the phenotypes variables.


    /Biomarkers

        Everything related to the biomarkers.

         /doc

              Documentation that explain what we have in the data.

              TODO: Explain each of these:

              Olink_Inf_FF1_LOD_Values.xlsx

              Olink_Inf_FF1_Metadata.xlsx

              Olink_Inf_FF1_PercentMissing.xlsx

              Olink_Inf_FF1_ProteinNames.xlsx


    /dataframe_ready

        This folder contain all the data that is clean already.

		There are two scrips that take care of the data:

		- One, read the original data, convert columns from "1" and "2" to "Men"
          and women, calculate connectivity in the network, and so on. Once is
          finish, it create the appropiated .csv files in this folder.

        - Two, read these .csv and load then into memory with the proper data
          structures so you can do plots and whatever you want with the data.


     /fakeData

         As the name suggest, this is just random data of phenotypes, friends,
         and everything else.

         It is use to test the plots and all the tools. It contain weird NAs,
         NULLs, and empty cells here and there on porpuse, to test that
         everything work at it should even with filthy data.

         "fakeDataWithNulls.csv"

             The fake phenotype table

         "fakeFriends.csv"

             The fake friends table

         "fakePhenotypeTable.csv"

              DEPECRATED, mark as candidate to delete.

     /phenotypes

        Everything related to the phenotypes. In general, is every patient info
        and every variable that is not biomarkers, aureus, or network specific.

		/doc

            The documentation about everything regarding these variables are
            saved here.

			"20180601-Komplett Metadata FF1.xls"

                Documentation regarding every variable. Keep in mind that there
                are about 1000 variables in here, and we don't have access to
                most of them, not do we care about them either. For example, the
                biological parents current relationship of each patient.

/reports

	A folder where different reports, automatic or manually, are stored.

    "weird.txt"

        A bunch of things found in the data that are weird and need
        clarification. This is for example, people who report no physical
        activity during the day also report going to a sport program school or
        doing hard sports in another different question.


/results

    In here the results of the scripts are saved. Each subfolder here has an
    specific naming system that goes as:
 
    /YYYYMMDD_HHMMSS

    which is the timestamp of when you run the script. Everytime you run the
    script a new folder is created and everything dumped there.

    /Examples

    Results that are saved to highlight something in a particular meeting,
    document or whatever.

/src

	It has all the code related to the project.

	aureus.R

        The script that analyze everything that has to do with the S.Aureus
        variables.
   
        Notice though that there is another file called "network.R" that analyze
        everything to do with the network. If you want to see the network
        analysis of for example the SPA typing, you will find it in the AUREUS.R
        file and not in the other one, as I condiser that is very specific
        question related to aureus and not the network itself.

        With that says, there might be things that you expect to find in the 
        aureus analysis, but might be done somewhere else.

	automaticAnalysis.R

        Running this script, you will get automatic results for all the
        variables. Chances are that the plots colors are going to be ugly, the
        titles are also set comically automatic, and so for. But the results are
        legit.

        The interesting part of this script is that you don't need to think
        anything for any variables and just get a list of things that are good
        candidates to into with a more manual approach.

	dataCleaning.R

		This is the file that takes the original data found in /data, transform
        it, and save it into the /dataframe_ready folder for everyone ejoyment.

         In the folder "/data" there is all the data that is use for doing this.
         It contains two types of data, the real data, and fake datasets used
         for testing. Each is named in the code accordingly.

         Please read further the description of the /data folder.

    constants.R

         This files contains the constant variables. They are very irrelevant as
         they are just a list of colors, plot names, file paths, and so for.

         This contains the constants for two things:

              - The plot for testing the plotting library

              - The plot for getting real results.

	generalAnalysis.R

		This script give you an overview of your whole data that is not
        related to anything in particular. For example, sex, age, BMI, smoker
        status and so on.

        Further analysis, like for example comparing the network for men and
        woman, is done the network.R script. S.Aureus will be done in the
        aureus.R script, and so for.

	load.R

        This load the data from the /dataframe_ready into dataframes, whatever
        it is real data or fake data. It also build proper data structures,
        like for example making the graph objects for the igraph library and so
        on.

	network.R

        The script that does anything that is related to network analysis.

	testingPlots.R

        A script for testing the plots in the tools.R file.

	tools.R

        General tools for everything, but specially related to the plots.
        (notice that the plot will eventually have their own script)


README.txt

       This file.
