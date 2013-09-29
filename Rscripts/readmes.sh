
# copy explanation.Rmd to be Cardoso's readme
cp explanation.Rmd ../Experimental.Schedules/Cardoso/README.Rmd
# knit
cd ../Experimental.Schedules/Cardoso/
Rscript -e "library(knitr); knit(input='README.Rmd',output='README.md')"
#pandoc
#~/.cabal/bin/pandoc -s -S ../Experimental.Schedules/Cardoso/README.md -o ../Experimental.Schedules/Cardoso/README.pdf --latex-engine=xelatex
cd ../../Rscripts