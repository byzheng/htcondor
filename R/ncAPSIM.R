# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   9:40 AM Tuesday, 8 May 2012
# * Copyright: AS IS
# *

# Functions read netCDF files for APSIM output


#' Synonymy with nc_open in ncdf4 package
#' @param filename Name of the existing netCDF file to be opened.
#' @param ... Other arguments pass to nc_open
#' @export
ncOpen <- function(filename, ...)
{
    library(ncdf4)
    nc <- nc_open(filename, ...)
    return(nc)
}
#' Synonymy with nc_close in ncdf4 package
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
#' @export
ncClose <- function(nc)
{
    nc_close(nc)
}


#' Get all dimension names for one variables
#'
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
#' @var variable name
ncGetDimNames <- function(nc, var)
{
    vars <- nc$var[[var]]
    dim_name <- NULL
    for (i in seq(length = vars$ndims))
    {
        dim_name <- c(dim_name, vars$dim[[i]]$name)
    }
    return(dim_name)
}

#' Obtain factor levels from a netCDF file
#'
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
#' @param factorname The leves of factor name will be returned. All levels will be returned if NA is specified.
#' @param split character called by strsplit
ncFactorLevels <- function(nc, factorname = NA, split = ' *, *')
{
    if(is.na(factorname))
    {
        factor_levels <- NULL
        for (i in seq(along = nc$dim))
        {
            dim_name <- as.character(nc$dim[[i]]$name)
            nc_var <- nc$var[[sprintf('%s_lbl', dim_name)]]
            if (is.null(nc_var))
            {
                next
            }
            factor_levels[[dim_name]] <- ncvar_get(nc, nc_var)
        }
        return (factor_levels)
    } else
    {
        nc_var <- nc$var[[sprintf('%s_lbl', factorname)]]
        if (is.null(nc_var))
        {
            return (NULL)
        }
        return(ncvar_get(nc, nc_var))
    }
    return (NULL)
}

#' Obtain trait list from a netCDF file
#'
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
#' @param attname The default attribute name of trait list 
#' @param split character called by strsplit
ncTraitList <- function(nc, attname = 'TraitList', split = ' *, *')
{
    library(ncdf4)
    factors <- ncatt_get(nc, 0, attname)$value
    if (factors == 0)
    {
        return(NA)
    }
    return (strsplit(factors, split)[[1]])
}


#' Convert factor levels to index 
#' @param nc nc
#' @param filename filename
#' @param title title
#' @param split split
#' @param ... ...
#' @export
ncReadApsimIndex <- function(nc, filename, title = 'Title', split = ',', ...)
{
    if (is.character(filename))
    {
        res_title <- readApsimHead(filename, title, split, ...)
    } else
    {
        res_title <- filename
    }
    res <- NULL
    for (i in seq(length = length(nc$dim)))
    {
        f_name <- as.character(nc$dim[[i]]$name)
        if (f_name %in% c('Year', 'str_len'))
        {
            next
        }
        f_pos <- match(f_name, res_title$name)
        
        if (is.na(f_pos))
        {
            stop(paste(f_name, ' does not existe', sep = ''))
        }
        nc_var <- nc$var[[sprintf('%s_lbl', f_name)]]
        if (is.null(nc_var))
        {
            stop(paste(f_name, '_lbl don\'t exist.', sep = ''))
        }
        nc_levels <- ncvar_get(nc, nc_var)
        nc_levels_v <- seq(along = nc_levels)
        i_res <- as.numeric(nc_levels_v[match(res_title$value[f_pos], nc_levels)])
        if (is.na(i_res))
        {
            stop(sprintf('Cannot find values for %s', res_title$value[f_pos]))
        }
        res <- c(res, i_res)
    }
    return(res)
}

#' Create netcdf file for APSIM
#'
#' @param filename The filename to save netcdf file.
#' @param years A vector for year demension.
#' @param factors A list for all dimensions in the netcdf.
#' @param traits A vector of traits in the netcdf.
#' @param is.close Whether to close netcdf file. 
#' @param global.att Global attribures
#' @param ... Attributes of traits, e.g. unit
#' If false, a nv variable will be returned.
#' @examples
#' @export
ncCreateApsim <- function(filename, years, factors, 
    traits,
    is.close = TRUE,
    global.att = NULL,
    ...)
{
    library(ncdf4)
    for (k in seq(along = factors))
    {
        factors[[k]] <- factor(factors[[k]],
            levels = factors[[k]], labels = factors[[k]])
    }
    
    nc_dim <- list(NULL)
    nc_vars <- list(NULL) 
    
    # str_len dimension
    factors_level <- lapply(factors, levels)
    str_len <- max(c(4, as.numeric(lapply(factors_level, 
        function(x) max(nchar(x))))))
    str_dim <- ncdim_def('str_len', '', 1:str_len,
            create_dimvar = FALSE)
    # nc_vars[[1]] <- str_dim
    
    # Year dimension and variable
    year_dim <- ncdim_def('Year', '', 1:length(years),
            create_dimvar = FALSE)
    nc_dim[[1]] <- year_dim
    nc_vars[[1]] <- ncvar_def('Year_lbl', '', list(str_dim, year_dim),
            prec = 'char')
    f_names <- names(factors)
    # Create factors lables
    for (k in seq(along = f_names))
    {
        l_dim <- ncdim_def(f_names[k], '', seq(along = factors_level[[k]]),
            create_dimvar = FALSE)
        nc_dim[[length(nc_dim) + 1]] <- l_dim
        
        l_var <- ncvar_def(sprintf('%s_lbl', f_names[k]), 
            '', list(str_dim, l_dim), prec = 'char')
        nc_vars[[length(nc_vars) + 1]] <- l_var
    }
    
    # Create variables of traits
    for (k in seq(along = traits))
    {
        l_var <- ncvar_def(traits[k], '', nc_dim,
            prec = 'float', longname = traits[k], missval = NA,
            compression = 9)
        nc_vars[[length(nc_vars) + 1]] <- l_var
    }
    
    # Create new netcdf file
    ncnew <- nc_create(filename, as.list(nc_vars))
    
    # Create attribute and variables
    nc_redef(ncnew)
    
    
    # Create coordinate attribute
    for (k in seq(along = traits))
    {
        ncatt_put(ncnew, traits[k], 'coordinates', 
            paste(sprintf('%s_lbl', rev(c('Year', f_names))), collapse = ' '), 
            prec = 'text', definemode = TRUE)
    }
    
    # Add other attribures
    atts <- list(...)
    atts_name <- names(atts)
    for (k in seq(along = atts))
    {
        att <- rep(atts[[k]], length = length(traits))
        for (j in seq(along = traits))
        {
            ncatt_put(ncnew, traits[j], atts_name[k], 
                att[j], prec = 'text', definemode = TRUE)
        }
    }
    
    # Add global attribute
    ncatt_put(ncnew, 0, 'Conventions', 
        'CF-1.6', prec = 'text',
        definemode = TRUE)
    ncatt_put(ncnew, 0, 'TraitList', 
        paste(traits, collapse = ','),
        prec = 'text',
        definemode = TRUE)
    if (!is.null(global.att))
    {
        att_names <- names(global.att)
        for (k in seq(along = att_names))
        {
            ncatt_put(ncnew, 0, att_names[k], 
                global.att[[k]], prec = 'text',
                definemode = TRUE)
        }
    }
    
    
    nc_enddef(ncnew)
    
    # Add factor labels
    for (k in seq(along = f_names))
    {
        ncvar_put(ncnew, sprintf('%s_lbl', f_names[k]), 
            factors_level[[k]], start = c(1, 1), 
            count = c(str_len, length(factors_level[[k]])))
    }
    
    ncvar_put(ncnew, 'Year_lbl', as.character(years)) 

    if (is.close)
    {
        nc_close(ncnew)
    } else
    {
        return(ncnew)
    }
}


#' Read APSIM out file
#' @param nc nc
#' @param filenames filenames
#' @param start start
#' @param res.array res.array
#' @param yearvar yearvar
#' @param traits traits
#' @param ... extra arguments
#' @export
ncReadApsimOut <- function(nc, filenames, start, 
    res.array, yearvar = 'Year', traits = NULL, ...)
{
    years <- as.numeric(ncFactorLevels(nc, 'Year'))
    n_years <- length(years)
    if (is.null(traits))
    {
        traits <- ncTraitList(nc)
    }
    nc_res <- as.list(NULL)
    nc_res$start <- c(1, start)
    nc_res$count <- dim(res.array)
    for (i in seq(along = traits))
    {
        nc_res[[traits[i]]] <- res.array
    }
    errors <- NULL
    for (i in seq(along = filenames))
    {
        if(!file.exists(filenames[i]))
        {
            warning(paste(filenames[i], ' does not existed.', sep = ''))
            errors <- c(errors, filenames[i])
            next
        }
        
        type <- tolower(
            gsub('.*\\.(.*)', '\\1', filenames[i]))
        if (type == 'out')
        {
            res <- readApsimOut(filenames[i])
            if (is.null(res))
            {
                warning('Empty output files')
                next
            }
            if (is.null(res[[yearvar]]))
            {
#                 file.remove(filenames[i])
                errors <- c(errors, filenames[i])
                warning(paste('yearvar ', yearvar, ' is not correct. Please check it.', sep = ''))
                next
            }
            
            # Get rid of other years
            res <- res[res[[yearvar]] %in% years,]
            # fill the empty years
            pos <- match(years, res[[yearvar]])
            res <- res[pos,]
            res[[yearvar]] <- years
        
            if(nrow(res) != length(years))
            {
                # warning('Results are not for all years')
                # next
            }
            traits <- traits[traits %in%  names(res)]
            res_index <- ncReadApsimIndex(nc, filenames[i], ...)
            # res_index <- ncReadApsimIndex(nc, filenames[i])
            res_index <- res_index - start + 1
            res_index <- cbind(1:n_years, 
                matrix(rep(res_index, each = n_years), 
                    dim <- c(n_years, length(res_index) + 1)))
            for (j in seq(along = traits))
            {
                nc_res[[traits[j]]][res_index] <- res[[traits[j]]]
            }
        } else if (type %in% c('rdata', 'rds'))
        {
            tryCatch(
            {
                res <- NULL
                if (type == 'rdata')
                {
                    envir = new.env()
                    vars <- load(filenames[i], envir)
                    assign('res', 
                        get(vars[1], envir = envir))
                    rm(envir)
                } else if(type == 'rds')
                {
                    res <- readRDS(filenames[i])
                }
                if (is.null(res))
                {
                    warning('Empty output files')
                    next
                }
                if (!is.null(res$head))
                {
                  temp <- res
                  res <- as.list(NULL)
                  res[[1]] <- temp
                }
                for (j in seq(along = res))
                  {
                    res_j <- res[[j]]
                    out <- res_j$out
                    # Check yearvar
                    if (is.null(out[[yearvar]]))
                      {
                          errors <- c(errors, filenames[i])
                          warning(paste('yearvar ', yearvar, ' is not correct. Please check it.', sep = ''))
                          break
                      }
                    # Get rid of other years
                      out <- out[out[[yearvar]] %in% years,]
                    # fill the empty years
                    pos <- match(years, out[[yearvar]])
                    out <- out[pos,]
                    out[[yearvar]] <- years
                
                    
                      if(nrow(out) != length(years))
                      {
                          warning('Results are not for all years')
                          next
                      }
                      traits <- traits[traits %in%  names(out)]
                      res_index <- ncReadApsimIndex(nc, res_j$head)
                      res_index <- res_index - start + 1
                      res_index <- cbind(1:n_years, 
                          matrix(rep(res_index, each = n_years), 
                              dim <- c(n_years, length(res_index) + 1)))
                      for (k in seq(along = traits))
                      {
                          nc_res[[traits[k]]][res_index] <- as.numeric(out[[traits[k]]])
                      }
                  }
            }, error = function(e)
            {
                warning(paste0(filenames[i], 
                    ' cannot be red with error "', e, '"'))
                errors <- c(errors, paste0(filenames[i], ': ', e))
#                 file.remove(filenames[i])
                assign('errors', errors)
            })
        }
    }
    nc_res$errors <- errors
    
    return(nc_res)
}

#' Update netcdf file 
#' @param nc nc
#' @param outputs outputs
#' @export
ncUpdateApsim <- function(nc, outputs)
{
    library(ncdf4)
    traits <- ncTraitList(nc)
    traits <- traits[traits %in% names(outputs)]
    years <-  length(ncFactorLevels(nc, 'Year'))
    start <- outputs$start
    count <- outputs$count
    for (j in seq(along = traits))
    {
        ncvar_put(nc, traits[j], outputs[[traits[j]]], 
            start = start, count = count)
    }
}


#' Obtain start according dimension levels
#' @param nc nc
#' @param ... extra arguments
#' @export
ncGetStart <- function(nc, ...)
{
    factors <- list(...)
    f_names <- names(factors)
    res <- NULL
    for (i in seq(length = length(nc$dim)))
    {
        f_name <- as.character(nc$dim[[i]]$name)
        if (f_name %in% c('Year', 'str_len'))
        {
            next
        }
        if (f_name %in% f_names)
        {
            nc_levels <- ncFactorLevels(nc, f_name)
            res <- c(res, match(factors[[f_name]], nc_levels))
        } else
        {
            res <- c(res, 1)
        }
    }
    return(res)
}

# #' Obtain start and count according dimension levels
# ncGetIndex <- function(nc, ...)
# {
    # factors <- list(...)
    # f_names <- names(factors)
    # start <- NULL
    # count <- NULL
    # for (i in seq(length = length(nc$dim)))
    # {
        # f_name <- as.character(nc$dim[[i]]$name)
        # nc_levels <- ncFactorLevels(nc, f_name)
        # if (f_name %in% f_names)
        # {
            # start <- c(start, match(factors[[f_name]], nc_levels))
            # count <- c(count, 1)
        # } else
        # {
            # start <- c(start, 1)
            # count <- c(count, length(nc_levels))
        # }
    # }
    # return(list(start = start, count = count))
# }


#' Obtain trait value from a netCDF file
#'
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
#' @param traits The trait name will be returned
#' @param ... Other arguments for factors. 
#' All variables will be returned in this factor if a factor is not specified.
#' @export
ncGetTraits <- function(nc, traits = NULL, ...)
{
    library(ncdf4cf)
    
    nc_traits <- ncTraitList(nc)
    if (is.null(traits))
    {
        traits <- nc_traits
    } else
    {
        traits <- nc_traits[match(traits, nc_traits)]
        if (sum(is.na(traits)) > 0)
        {
            stop(paste(paste(traits[is.na(traits)], collapse = ', '), 
                'can not be found in the traits.'))
        }
    }
    
    missing_var <- rep(NA, length(traits))
    for (i in seq(along = traits))
    {
        missing_var[i] <- nc$var[[i]]$missval
    }
    
    library(abind)
    res <- NULL
    for (i in seq(along = traits))
    {
        values <- cfvar_get(nc, traits[i], ...)
        if (!is.na(missing_var[i]))
        {
            values[abs(values - missing_var[i]) < .Machine$double.eps ^ 0.5] <- NA
            values[values > missing_var[i]] <- NA
        } else
        {
            values[is.na(values)] <- NA
        }
        if (length(traits) > 1)
        {
            res[[i]] <- values
        } else
        {
            res <- values
        }
    }
    if (length(traits) > 1)
    {
        res <- do.call(abind, c(res, along = length(dim(values)) + 1))
        f_dimnames <- dimnames(values)
        f_dimnames$Trait <- traits
        dimnames(res) <- f_dimnames
    }
    return(res)
}


# Subset from some nc files
#' Obtain a subset from several nc files.
#'
#' @param ncs A vector of nc files
#' @param filename Filename for new nc file
#' @param traits A vector of traits will be retured. All traits will be return if NULL
#' @param ... Dimensions will be returned
#' @export
ncSubset <- function(ncs, filename, traits = NULL, ...)
{
    # open netcdf file and read traits list
    ncids <- as.list(NULL)
    nc_traits <- as.list(NULL)
    for (i in seq(along = ncs))
    {
        ncid <- ncOpen(ncs[i])
        ncids[[i]] <- ncid
        nc_traits[[i]] <- ncTraitList(ncid)
    }
    
    outputs <- as.list(NULL)
    # For each traits
    for (i in seq(along = traits))
    {
        trait_nc <- NA
        # find nc file for this trait
        for (j in seq(along = ncs))
        {
            if (traits[i] %in% nc_traits[[j]])
            {
                trait_nc <- ncids[[j]]
                break
            }
        }
        if (is.na(trait_nc[1]))
        {
            stop(paste('Can not find the nc file included trait', traits[i]))
        }
        
        nc_subset <- ncGetTraits(trait_nc, traits[i], ...)
        outputs[[traits[i]]] <- nc_subset
    }
    
    new_dims <- dimnames(outputs[[traits[1]]])
    
    outputs$start <- rep(1, length(new_dims) - 1)
    outputs$count <- as.numeric(lapply(new_dims, length))[-length(new_dims)]
    
    new_args <- list(filename = filename, years = as.numeric(new_dims[['Year']]),
        factors = new_dims[!(names(new_dims) %in% c('Year', 'Traits'))], 
        traits = traits,
        is.close = FALSE)
        
    new_nc <- do.call(ncCreateApsim, new_args)
    ncClose(new_nc)
    new_nc <- nc_open(filename, write = TRUE)
    ncUpdateApsim(new_nc, outputs)
    for (i in seq(along = ncs))
    {
        ncClose(ncids[[i]])
    }
    ncClose(new_nc)
}

#' 
#' #' Calculate index for variables of applying a function
#' #'
#' #' @param nc An object of class ncdf4 (as returned from ncOpen), indicating what file to read from.
#' #' @param depth The depth for variables of applying a function.
#' ncCalculateApplyIndex <- function(nc, depth = length(nc$dim) - 1)
#' {
#'     
#' }
#' 
#' #' Return index for variables of applying a function
#' #'
#' #' @param nc An object of class ncdf4 (as returned from ncOpen), indicating what file to read from.
#' #' @param ... Other arguments to specify variables of dimensions 
#' ncGetApplyIndex <- function(nc, ...)
#' {
#'     
#' }



