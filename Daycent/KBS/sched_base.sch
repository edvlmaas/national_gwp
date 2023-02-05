1850          Starting year ## start with assumed ground-breaking for agriculture until intensification
1988          Last year
site.100  Site file name
0             Labeling type ## all defaults turned off
-1            Labeling year
-1.00         Microcosm
-1            CO2 Systems
-1            pH effect
-1            Soil warming
0             N input scalar option (0 or 1)
0             OMAD scalar option (0 or 1)
0             Climate scalar option
1             Initial system
C1            Initial crop ## low-yield corn
              Initial tree

Year Month Option
1       Block ## Corn, low yield, no fertilizer
1875    Last year
1       Repeats # of years
1850    Output starting year
12       Output month
1  Output interval
F       Weather choice
basic_eq.wth
1 111 CULT K     ## April 21 Moldboard plow
1 121 CULT H     ## May 1 Disk
1 121 CULT D     ## May 1 Cultivate
1 124 CROP C1    ## May 4
1 124 PLTM       ## May 4 Plant corn
1 177 FERT (0.75N)	## June 26
1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw
-999 -999 X
2       Block ## Higher-yielding corn with fertilizer
1900    Last year
1       Repeats # of years
1876    Output starting year
12       Output month
1  Output interval
C       Weather choice ## Continue
1 111 CULT K     ## April 21 Moldboard plow
1 121 CULT H     ## May 1 Disk
1 121 CULT D     ## May 1 Cultivate
1 124 CROP C3    ## May 4
1 124 PLTM       ## May 4
1 177 FERT (1.5N)	## June 26
1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw
-999 -999 X
3       Block ## Higher-yielding corn with fertilizer
1920    Last year
1       Repeats # of years
1901    Output starting year
12       Output month
1  Output interval
C       Weather choice ## Continue
1 111 CULT K     ## April 21 Moldboard plow
1 121 CULT H     ## May 1 Disk
1 121 CULT D     ## May 1 Cultivate
1 124 CROP C3    ## May 4
1 124 PLTM       ## May 4
1 177 FERT (2.2N)	## June 26
1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw
-999 -999 X
4       Block ## Higher-yielding corn with fertilizer
1949    Last year
1       Repeats # of years
1921    Output starting year
12       Output month
1  Output interval
C       Weather choice ## Continue
1 111 CULT K     ## April 21 Moldboard plow
1 121 CULT H     ## May 1 Disk
1 121 CULT D     ## May 1 Cultivate
1 124 CROP C4    ## May 4
1 124 PLTM       ## May 4
1 177 FERT (2.2N)	## June 26
1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw
-999 -999 X
5       Block ## Higher-yielding corn with fertilizer, add soybean to rotation
1959    Last year
2       Repeats # of years
1950    Output starting year
12       Output month
1  Output interval
F       Weather choice ## Start weather over at 1950
basic_eq.wth
1 138 CULT K     ## May 18 Moldboard plow
1 145 CULT H     ## May 25 Disc
1 145 CULT D     ## May 25 Cultivate
1 149 CROP SYBN3 ## May 29
1 149 PLTM       ## May 29 Plant soybean
1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw
2 111 CULT K     ## April 21 Moldboard plow
2 121 CULT H     ## May 1 Disk
2 121 CULT D     ## May 1 Cultivate
2 124 CROP C4    ## May 4
2 124 PLTM       ## May 4
2 177 FERT (3.5N)	## June 26
2 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw
-999 -999 X
6       Block ## Higher-yielding corn with fertilizer, continue soybean
1969    Last year
2       Repeats # of years
1960    Output starting year
12       Output month
1  Output interval
C       Weather choice ## Continue weather from previous block
1 138 CULT K     ## May 18 Moldboard plow
1 145 CULT H     ## May 25 Disc
1 145 CULT D     ## May 25 Cultivate
1 149 CROP SYBN3 ## May 29
1 149 PLTM       ## May 29 Plant soybean
1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw
2 111 CULT K     ## April 21 Moldboard plow
2 121 CULT H     ## May 1 Disk
2 121 CULT D     ## May 1 Cultivate
2 124 CROP C6    ## May 4
2 124 PLTM       ## May 4
2 177 FERT (4.5N)	## June 26
2 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw
-999 -999 X
7       Block ## Higher-yielding corn with fertilizer, continue soybean
1979    Last year
2       Repeats # of years
1970    Output starting year
12       Output month
1  Output interval
C       Weather choice ## Continue weather from previous block
1 138 CULT K     ## May 18 Moldboard plow
1 145 CULT H     ## May 25 Disc
1 145 CULT D     ## May 25 Cultivate
1 149 CROP SYBN2 ## May 29
1 149 PLTM       ## May 29 Plant soybean
1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw
2 111 CULT K     ## April 21 Moldboard plow
2 121 CULT H     ## May 1 Disk
2 121 CULT D     ## May 1 Cultivate
2 124 CROP C6    ## May 4
2 124 PLTM       ## May 4
2 177 FERT (6N)	## June 26
2 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw
-999 -999 X
8       Block ## Higher-yielding corn with fertilizer, continue soybean
1987    Last year
2       Repeats # of years
1980    Output starting year
12       Output month
1  Output interval
C       Weather choice ## Continue weather from previous block
1 138 CULT K     ## May 18 Moldboard plow
1 145 CULT H     ## May 25 Disc
1 145 CULT D     ## May 25 Cultivate
1 149 CROP SYBN2 ## May 29
1 149 PLTM       ## May 29 Plant soybean
1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw
2 111 CULT K     ## April 21 Moldboard plow
2 121 CULT H     ## May 1 Disk
2 121 CULT D     ## May 1 Cultivate
2 124 CROP C6    ## May 4
2 124 PLTM       ## May 4
2 177 FERT (8N)	## June 26
2 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw
-999 -999 X
9       Block ## Just soybean
1988    Last year
1       Repeats # of years
1988    Output starting year
12       Output month
1  Output interval
C       Weather choice ## Continue weather from previous block
1 138 CULT K     ## May 18 Moldboard plow
1 145 CULT H     ## May 25 Disc
1 145 CULT D     ## May 25 Cultivate
1 149 CROP SYBN1 ## May 29
1 149 PLTM       ## May 29 Plant soybean
1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw
-999 -999 X
