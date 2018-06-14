#  File networkDynamic/R/zzz.R
#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2012 the statnet development team
######################################################################
# .onLoad is run when the package is loaded with library(networkDynamic)
#
######################################################################

.onLoad<- function(lib, pkg){
    packageStartupMessage(mkStartupMessage('networkDynamic'))
    packageStartupMessage("Copyright (c) 2009 Carter T. Butts")
    packageStartupMessage('Type help("networkDynamic-package") to get started.')
}

# Functions below copied from statnet.common.  Putting it here for now because statnet.common is not on CRAN, so can't have the dependencey yet. 
mkStartupMessage <- function(pkgname){
  require(utils) # need this for packageDescription() and person() 
}

## If EXPR is NULL, return NULLV, otherwise return EXPR.
NVL <- function(EXPR, NULLV) if(!is.null(EXPR)) EXPR else NULLV