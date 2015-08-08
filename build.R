library(devtools)

unload(inst("Rgitbook"))
remove.packages("Rgitbook")

install.packages("~/Projects/Rgitbook/", type = "source", repos = NULL)
library(Rgitbook)

#initGitbook()
cleanGitbook()

buildGitbook()
openGitbook()
