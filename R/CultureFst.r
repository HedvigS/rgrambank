#' Calculates Cultural fixation scores, as specified in Muthukrishna et al (2020)
#'
#' @param d a NxM matrix of N observations for M traits, the first column must consist of population names; columns are given "trait" names, within which are variants of each trait. If d is not defined, ValueTable_long  and PopTable need to be defined
#' @param ValueTable_long dataframe with columns ID, Parameter_ID and Value. If ValueTable_long is defined, d and loci must be NULL
#' @param PopTable dataframe with columns ID and Pop_ID. If PopTable is defined, d must be NULL
#' @param loci character vector that name the traits for which the fst is to be computed. 'loci' must be NULL when 'ValueTable_long' is provided. loci are derived from 'ValueTable_long' directly if it is supplied.
#' @param type Either a numeric vector of length 1 (0 = discrete/categorical, 1 = quantiatative/ordinal) or a named numeric vector of the same length as "loci" (or number of unique values in Parameter_ID in ValueTable_long) indicating what type of each trait it is. If the argument is of length 1, that type will be applied to all loci. If the argument is longer than 1, note that the names of the vector must be the vector loci, i.e. make sure that row.names(type) <- loci
#' @param bootstrap logical, if TRUE, telling the program to compute bootstrapped standard errors and confidence intervals
#' @param no.samples numeric vector of length 1. Number of resamples in the bootstrap. Default is 100. Only used if bootstrap is TRUE.
#' @param label character string that will be part of the output .rdata file name  (saved as "label_Fst.rdata"). If NULL (the default), no file is saved.
#' @param verbose logical. If TRUE, reports progress messages including the current bootstrap sample number. Default is TRUE.
#' @return A named list containing:
#'   \itemize{
#'     \item \code{fst.loci} list of pairwise Fst values per trait (when multiple loci)
#'     \item \code{mean.fst} matrix of mean pairwise Fst values. If bootstrap is TRUE, the upper diagonal contains bootstrapped standard errors and the lower diagonal contains Fst estimates
#'     \item \code{mean.fst.confint} matrix with bootstrapped 95\% confidence intervals in the upper diagonal, if bootstrap is TRUE
#'     \item \code{loci} character vector of trait names used
#'     \item \code{pops} character vector of population names
#'     \item \code{sample.size} named numeric vector of sample sizes per population
#'     \item \code{boot} list of bootstrap results (pairs, se, mean, quantiles, estimates), if bootstrap is TRUE
#'   }
#' @note This function is adapted from Muthukrishna et al (2020). Please cite the original article when used in publications.
#' @author Original function: Michael Muthukrishna, Adrian V. Bell, Joseph Henrich, Cameron M. Curtin, Alexander Gedranovich, Jason McInerney & Braden Thue. Adaptation for rgrambank: Hedvig Skirgård
#' @export  
#' 
#' 

#'@references Muthukrishna, M., Bell, A. V., Henrich, J., Curtin, C. M., Gedranovich, A., McInerney, J., & Thue, B. (2020). "Beyond Western, Educated, Industrial, Rich, and Democratic (WEIRD) Psychology: Measuring and Mapping Scales of Cultural and Psychological Distance." Psychological Science, 0956797620916782. Published, 05/21/2020.
#https://journals.sagepub.com/doi/suppl/10.1177/0956797620916782

CultureFst <- function(d = NULL, 
                      ValueTable_long  = NULL,
                      PopTable = NULL,
                      loci = NULL, 
                      type  = NULL, 
                      bootstrap  = TRUE, 
                      no.samples  = 100, 
                      label = NULL, 
                      verbose = TRUE ){
  
  
  # ------- various argument checks, including creating d and loci if ValueTable_long and PopTable are defined ----------
  
  #the original function had the arguments d and loci. Users of the package rgrambank are likely more familiar with using long ValueTables and additional tables for information such as groups/populations. The function has been modified so that users can supply ValueTable_long and PopTable instead of d and loci. The function then renders d and loci correctly from those arguments. The old functionality is still preserved, users can us d and loci as before.
    
if ((is.null(d) || is.null(loci)) && (is.null(ValueTable_long) || is.null(PopTable))) {
    stop(
      "Either 'd' and 'loci' must be provided, or both 'ValueTable_long ' and 'PopTable' must be provided."
    )
  }
  
  if (!is.null(d) && !is.matrix(d)) {
    stop("'d' must be a matrix.")
  }
  
  
  if (!is.null(d) && anyNA(d[, 1])) {
    stop("The first column in 'd' cannot have NAs.")
  }
  
  if (!is.null(ValueTable_long ) && !is.data.frame(ValueTable_long )) {
    stop("'ValueTable_long ' must be a data frame.")
  }
  
  if (!is.null(PopTable) && !is.data.frame(PopTable)) {
    stop("'PopTable' must be a data frame.")
  }
  
  if (!is.null(PopTable) && anyNA(PopTable$Pop_ID)) {
    stop("The column 'Pop_ID' in 'PopTable' cannot contain NAs.")
  }
  
  # If ValueTable is provided, loci must be NULL
  if (!is.null(ValueTable_long) && !is.null(loci)) {
    stop(
      "'loci' must be NULL when 'ValueTable_long' is provided. ",
      "loci are derived from 'ValueTable_long' directly."
    )
  }
  
  # Check required columns in ValueTable_long
  if (!is.null(ValueTable_long)) {
    required_cols_ValueTable <- c("ID", "Parameter_ID", "Value")
    missing_cols_ValueTable  <- required_cols_ValueTable[
      !required_cols_ValueTable %in% colnames(ValueTable_long)
    ]
    if (length(missing_cols_ValueTable) > 0) {
      stop(
        "'ValueTable_long' is missing required column(s): ",
        paste(missing_cols_ValueTable, collapse = ", "),
        ". Required columns are: ID, Parameter_ID, Value."
      )
    }
  }
  
  # Check required columns in PopTable
  if (!is.null(PopTable)) {
    required_cols_PopTable <- c("ID", "Pop_ID")
    missing_cols_PopTable  <- required_cols_PopTable[
      !required_cols_PopTable %in% colnames(PopTable)
    ]
    if (length(missing_cols_PopTable) > 0) {
      stop(
        "'PopTable' is missing required column(s): ",
        paste(missing_cols_PopTable, collapse = ", "),
        ". Required columns are: ID, Pop_ID."
      )
    }
  }
  
if(is.data.frame(ValueTable_long)){
  loci = ValueTable_long$Parameter_ID |> unique()
}
  
# The original function used the argument "type" which was a named vector of the same length as number of features to compare over (loci). In the rgrambank version, users can set type to just one value (0 or 1) if all loci/features are of the same type. The named vector is then created. The old behaviour is preserved, users can still give a named vector for type.

  if(is.null(type)){
  stop("'type' needs to be defined.")
} else{
  
  if (length(type) == 1) {
    
    # scalar — must be 0 or 1
    if (!type %in% c(0, 1)) {
      stop("'type' must be 0 (discrete/categorical) or 1 (quantitative/ordinal).")
    }

    #creating a named numeric vector where all values are the same
    type <- rep(type, length(loci))
    names(type) <- loci
    
  } else {
    # vector — all values must be 0 or 1
    if (!all(type %in% c(0, 1))) {
      stop("All values in 'type' must be 0 (discrete/categorical) or 1 (quantitative/ordinal).")
    }
    
    # must be named
    if (is.null(names(type))) {
      stop("'type' has length > 1 but has no names. The vector needs to be named, matching loci/ValueTable_long$Parameter_ID")
    }
    
    # names must exactly match loci (no missing, no extra, no duplicates)
    if (length(type) != length(loci)) {
      stop(
        "'type' has length ", length(type), " but 'loci' has length ", length(loci), ".",
        " They must be the same length."
      )
    }
    
    if (anyDuplicated(names(type)) > 0) {
      stop("Names of 'type' must not contain duplicates.")
    }
    
    if (!setequal(names(type), loci)) {
      stop("Names of 'type' must exactly match 'loci'.")
    }
  }
}
  
  
if(is.null(d)){

  # merge ValueTable_long and PopTable
  merged <- merge(ValueTable_long, PopTable, by = "ID", all.x = TRUE)
  
  # pivot wider — reshape from long to wide
  d <- stats::reshape(
    merged,
    idvar     = c("ID", "Pop_ID"),
    timevar   = "Parameter_ID",
    v.names   = "Value",
    direction = "wide"
  )
     
  # clean up column names (reshape adds "Value." prefix)
  colnames(d) <- gsub("^Value\\.", "", colnames(d))
  
  # drop ID column
  d <- d[, !colnames(d) %in% "ID"]
  
  d <- as.matrix(d)
}
  
  
counts_per_pop <- table(d[,1])
  
  if(any(counts_per_pop < 2)){
    small_pops <- names(counts_per_pop[counts_per_pop < 2])
    stop(
      "The following population(s) have fewer than 2 members and therefore cannot be used for calculating Fst: ",
      paste(small_pops, collapse = ", "),
      ". Please remove these populations, or reassign the observations to a different group."
    )
  }
  
  # ------- function to compute an Fst for a single trait ----------
  Fst.loci = function( d, l ){
    # d is the data matrix
    # l is the name of the trait	
    # set up vectors for the numerator
    # and denominator for the fst calculation
    fst_num = fst_den = rep(0, dim(pair)[1] )
    # find out the sample size for each population
    sample.size = sapply( pops, function(z) sum( d[ is.na(d[,l])==F,1]==z ) )
    # condition on whether it is a discrete or quantitative character
    if( type[[l]]==1 ){ # quantitative character
      # number of pairs
      npairs <- dim(pair)[1]
      print( paste( "q trait", l ) )
      # find total variance
      totalvar = sapply( 1:npairs, function(y){ w = pair[y,2]; yo = pair[y,1]; stats::var( c( d[ d[,1]==yo,l], d[ d[,1]==w, l ] ), na.rm=T )} ) 
      # compute between-group variance
      # find global mean
      totalmean = sapply( 1:npairs, function(y){ w = pair[y,2]; yo = pair[y,1]; mean( c( d[ d[,1]==yo,l], d[ d[,1]==w, l ] ), na.rm=T )} )
      g1mn = sapply( 1:npairs, function(y){ yo = pair[y,1]; mean( d[ d[,1]==yo,l], na.rm=T )} ) # quantitative variance
      names(g1mn) = pair[,1]
      g2mn = sapply( 1:npairs, function(y){ w = pair[y,2]; mean( d[ d[,1]==w, l ], na.rm=T )} ) # quantitative variance
      names(g2mn) = pair[,2]
      bgvar = sapply( 1:npairs, function(y){ yo = pair[y,1]; w = pair[y,2]; ((g1mn[[yo]] - totalmean[[y]])^2 + (g2mn[[w]] - totalmean[[y]])^2 ) } )
      # fst is the between-group variance over the total variance
      fst = bgvar / totalvar
    }else{ # discrete character
      # count the unique variants    	
      polymorphs = as.character( unique(d[,l]) ); 
      polymorphs = polymorphs[is.na(polymorphs)==FALSE]
      
      # find the frequency of any variant per trait per pair of populations
      freq.within = lapply( pops, function(z) { 
        data = d[ d[,1]==z, ]
        freq = rep(0, length(polymorphs) )
        for( i in 1:length(polymorphs) ){
          freq[i] = sum( data[,l]==polymorphs[i], na.rm=T ) / 	sum( is.na(data[,l])==FALSE )
        }
        freq
      } )
      names(freq.within) = pops      
      
      for( i in 1:length(polymorphs) ){
        # total variance, weighted by sample size	
        ave.p = sapply( 1:dim(pair)[1], function(y){ w = pair[y,2]; yo = pair[y,1]; (freq.within[[yo]][i] * sample.size[yo] + freq.within[[w]][i] * sample.size[w] ) / ( sample.size[yo] + sample.size[w] ) } ) # weighted by sample size
        
        # between group variance
        varpi = sapply( 1:dim(pair)[1], function(y){ yo = pair[y,1]; w = pair[y,2]; ((freq.within[[yo]][i] - ave.p[[y]])^2 + (freq.within[[w]][i] - ave.p[[y]])^2 ) } )
        
        if( is.numeric(ave.p)==F ) break
        # fst for this allele
        fsti = varpi / ( ave.p*(1-ave.p ) )
        # fst across alleles up to this point
        fst_num = ifelse( ave.p>0 , ( ave.p*(1-ave.p ) ) * fsti + fst_num, fst_num )
        fst_den = ifelse( ave.p>0, ( ave.p*(1-ave.p ) ) + fst_den, fst_den )
      }
      fst = fst_num/fst_den 			     
    }    
    # return fst for this trait
    fst
  }
  
  # --------- function for calculating Fst means for one or multiple traits -------------
  Fst.gen = function( d.a ){	
    
    if( length(loci)>1 ){	
      # fst across all traits
      f.loci = sapply( loci, function(z) Fst.loci(d.a, z ) )
      fst.all.loci = lapply( 1:length(loci), function(z) f.loci[,z] ) # put it back in a list
      names(fst.all.loci) = loci		
      # rearrange results by pairs of countries in a symmetric table
      mean.fst = suppressWarnings( sapply( pops, function(w) sapply( pops, function(y) mean( sapply( loci, function(z) fst.all.loci[[z]][ pair[,1]==w & pair[,2]==y ] ), na.rm = TRUE ) ) ) )
      ans = list( fst.all.loci, mean.fst, loci, pops )
      names(ans) = c("fst.loci","mean.fst","loci","pops")
      
    }else{ 
      # case where only one trait
      res.one.loci = Fst.loci(d.a, loci)
      ans = list( sapply( pops, function(y) sapply( pops, function(w) res.one.loci[ pair[,1]==y & pair[,2]==w ] ) ) )
      ans[[1]][upper.tri(ans[[1]], diag=TRUE)] <- NA		
      names(ans) = c( "mean.fst" )
    }
    ans
  }
  
  # ----------------------------------------------------------
  # Bootstrap function for confidence intervals
  bootFst = function(){
    
    # function to generate a mean Fst for each sample
    sampleFst = function( i ){
      
      if(verbose == TRUE){
      cat(paste0("CultureFst is on ", i, " out of ", no.samples, " samples for bootstrapping confidence intervals.\n"))
        }
      
      subpops = subset( pops, sapply( 1:length(pops), function(z) any(pops[z]==pair) ) )
      index.sample = sapply( subpops, function(z){ set = which(d[,1]== z); sample( set, length(set), replace = TRUE ) } )
      index.sample = unlist( index.sample )
      ans = Fst.gen( d[index.sample,] )
      res = ans$mean.fst
      
      res }
    # resample
    btfst = lapply( 1:no.samples, sampleFst )	
    
    # rearrange results by population pair, return a vector
    npairs <- dim(pair)[1] # number of pairs
    bootDistr = sapply( 1:npairs, function(w) { 
      a = 0
      for( i in 1:length(btfst) ){ 
        a = c(a, btfst[[i]][pair[w,2],pair[w,1]] )
      } 
      a = unlist(a[-1])
      a } ) 
    
    # calculate standard errors, means, and quantiles
    Fst.se = sapply( pops, function(w) sapply( pops, function(y){ ifelse( any(pair[,2]==w & pair[,1]==y), sqrt( stats::var( bootDistr[,pair[,2]==w & pair[,1]==y ], na.rm = TRUE ) ), NA ) } ) )
    
    Fst.mean = sapply( pops, function(w) sapply( pops, function(y){ ifelse( any(pair[,2]==w & pair[,1]==y), mean( bootDistr[,pair[,2]==w & pair[,1]==y ], na.rm = TRUE ), NA ) } ) )
    
    Fst.confint = sapply( pops, function(w) sapply( pops, function(y) ifelse( any(pair[,2]==w & pair[,1]==y), paste( format( stats::quantile( bootDistr[,pair[,2]==w & pair[,1]==y ], prob = 0.025, na.rm = TRUE ), digits = 3),", ", format( stats::quantile( bootDistr[,pair[,2]==w & pair[,1]==y ], prob = 0.975, na.rm = TRUE ), digits = 3), sep = "" ), NA )  ) )
    
    ans = list( pair, Fst.se, Fst.mean, Fst.confint, bootDistr )
    names(ans) = c("pairs", "se","mean","quantiles","estimates" )
    ans
  }	
  
  # ---------------------------------------------------
  # subfunction calls and output	
  # all pair-wise combinations
  pair = t( utils::combn( as.character( unique(d[,1]) ), 2 ) )
  # population names
  pops = as.character(unique(d[,1]))
  # run fst calculation
  ans = Fst.gen(d)
  
  ans$sample.size = sapply( pops, function(z) sum( d[,1]==z ) )
  
  if( bootstrap==TRUE ){ # arrange output with bootstrap results
    ans$boot = bootFst()
    
    fill.bt = upper.tri( ans$mean.fst, diag = FALSE )
    # mean with standard errors in the upper diagonal
    btse = sapply( 1:length(pops), function(y) sapply( 1:length(pops), function(x) ifelse( fill.bt[x,y]==TRUE, ans$boot$se[x,y], ans$mean.fst[[x,y]] ) ) )
    colnames(btse) = pops; rownames(btse) = pops
    ans$mean.fst = btse
    
    # mean with quantiles in the upper diagonal
    ans$mean.fst.confint = sapply( 1:length(pops), function(y) sapply( 1:length(pops), function(x) ifelse( fill.bt[x,y]==TRUE, ans$boot$quantiles[x,y], ans$mean.fst[x,y] ) ) )
    colnames(ans$mean.fst.confint) = pops; rownames(ans$mean.fst.confint) = pops
  }	
  # save output to .rdata file	
  
  if(!is.null(label)){
    save(ans, file = paste( label, "_Fst.rdata", sep = "" ) ) 
  }
  if( bootstrap==TRUE ) print( ans$mean.fst.confint ) else print(ans$mean.fst)
  ans
}