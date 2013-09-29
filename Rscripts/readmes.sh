
# copy explanation.Rmd to be Cardoso's readme
cp explanation.Rmd ../Experimental.Schedules/Cardoso/README.Rmd
# knit
Rscript -e "library(knitr); knit(input='../Experimental.Schedules/Cardoso/README.Rmd',input='../Experimental.Schedules/Cardoso/README.md')"
#pandoc
#~/.cabal/bin/pandoc -s -S ../Experimental.Schedules/Cardoso/README.md -o ../Experimental.Schedules/Cardoso/README.pdf --latex-engine=xelatex