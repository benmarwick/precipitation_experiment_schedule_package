
# copy explanation.Rmd to be Cardoso's readme
cp explanation.Rmd ../Experimental.Schedules/Cardoso/README.Rmd
# knit
cd ../Experimental.Schedules/Cardoso/
Rscript -e "library(knitr); knit(input='README.Rmd',output='README.md')"
#pandoc
#~/.cabal/bin/pandoc -s -S README.md -o README.pdf --latex-engine=xelatex
cd ../../Rscripts


# copy explanation.Rmd to readme
cp explanation.Rmd ../Experimental.Schedules/Colombia/README.Rmd
# knit
cd ../Experimental.Schedules/Colombia/
Rscript -e "library(knitr); knit(input='README.Rmd',output='README.md')"
#pandoc
#~/.cabal/bin/pandoc -s -S README.md -o README.pdf --latex-engine=xelatex
cd ../../Rscripts


# copy explanation.Rmd to readme
cp explanation.Rmd ../Experimental.Schedules/CostaRica/README.Rmd
# knit
cd ../Experimental.Schedules/CostaRica/
Rscript -e "library(knitr); knit(input='README.Rmd',output='README.md')"
#pandoc
#~/.cabal/bin/pandoc -s -S README.md -o README.pdf --latex-engine=xelatex
cd ../../Rscripts




# copy explanation.Rmd to readme
cp explanation.Rmd ../Experimental.Schedules/Argentina/README.Rmd
# knit
cd ../Experimental.Schedules/Argentina/
Rscript -e "library(knitr); knit(input='README.Rmd',output='README.md')"
#pandoc
#~/.cabal/bin/pandoc -s -S README.md -o README.pdf --latex-engine=xelatex
cd ../../Rscripts


# copy explanation.Rmd to readme
cp explanation.Rmd ../Experimental.Schedules/FrenchGuiana/README.Rmd
# knit
cd ../Experimental.Schedules/FrenchGuiana/
Rscript -e "library(knitr); knit(input='README.Rmd',output='README.md')"
#pandoc
#~/.cabal/bin/pandoc -s -S README.md -o README.pdf --latex-engine=xelatex
cd ../../Rscripts

# copy explanation.Rmd to readme
cp explanation.Rmd ../Experimental.Schedules/Macae/README.Rmd
# knit
cd ../Experimental.Schedules/Macae/
Rscript -e "library(knitr); knit(input='README.Rmd',output='README.md')"
#pandoc
#~/.cabal/bin/pandoc -s -S README.md -o README.pdf --latex-engine=xelatex
cd ../../Rscripts