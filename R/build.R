# script called by Makefile for integration with RStudio

clean <- function() { Rgitbook::cleanGitbook() }
build <- function() { Rgitbook::buildGitbook() }
open <- function() {  Rgitbook::openGitbook() }
publish <- function() { Rgitbook::publishGitbook() }

args <- commandArgs(TRUE)
stopifnot(length(args) >= 1)

do.call(args[1], args=list())
