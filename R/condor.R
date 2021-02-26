# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   04:05 PM Thursday, 13 June 2013
# * Copyright: AS IS
# *


# Condor functions for APSIM

#' Generate the sim prefix with md5
#'
#' @param factors A data.frame for factors
#' @export
simPrefix <- function(factors)
{
    library(digest)
    v_names <- names(factors)
    sim_title <- as.character(apply(factors, 1, FUN = function(x)
        {
            x <- gsub(' ', '', x)
            # x <- gsub('/', '_', x)
            x <- paste(paste(v_names, x, sep = '='), collapse = ',')
            return(x)
        }))
    prefix <- unlist(lapply(sim_title, digest, algo = 'md5'))
    return(prefix = prefix)
}



#' Compress the input files 
#'
#' @param input list of files and folders
#' @param output The file of output
#' @param sevenz The path to 7z.exe
#' @export
compressInputs <- function(input, output, 
    sevenz = 'C:\\Program Files\\7-Zip\\7z.exe')
{
    cmd <- sprintf('"%s" a -t7z -m0=LZMA -mmt=on -mx9 -md=64m -mfb=64 -ms=4g -sccUTF-8 -sfx "-w%s" "%s" %s',
        sevenz, 
        dirname(output),
        output,
        paste(paste0('"', input, '"'), collapse = ' '))
    system(cmd)
}


#' Preparing task table for APSIM simulations
#'
#' @param project The project name in the ClusterRun
#' @param factors The factors list to generate simulations
#' @param sharedrive sharedrive
#' @param local local
#' @param r_code r_code
#' @param par_factor The parallel factors to write into database of ClusterRun
#' @param input_factor The factors to indicate input files
#' @param skip_factor The skip factors to be processed in the condor clients
#' @param r_script The path to Rscript.exe file
#' @param folder_prefix Output folder to store the out files of all simulations
#' @param ids The starting id of simulations
#' @export
clusterRunPreparingTask <- function(
    project, 
    factors,
    sharedrive,
    local,
    par_factor,
    input_factor, 
    skip_factor = NULL,
    folder_prefix = NULL,
    r_script = 'R\\bin\\i386\\Rscript.exe',
    r_code = 'RCode.R',
    ids = 0)
{
    library(digest)
    if (ids == 0)
    {
        # Clean all tasks in the condor system
        library(RClusterRun)
        con <- connectTaskDB()
        cleanTasks(con, project)
        disconnectTaskDB(con)
    }
    # Generate factors for several targets
    c_factor <- factors
    if (!is.null(skip_factor))
    {
        c_factor <- c_factor[!(names(c_factor) %in% skip_factor)]
    }
    
    c_par_factor <- c_factor[(names(c_factor) %in% par_factor)]
    c_par_factor <- expand.grid(c_par_factor, stringsAsFactors = FALSE)
    c_com_factor <- expand.grid(
        c_factor[!(names(c_factor) %in% par_factor)],
        stringsAsFactors = FALSE)
    
    # Generate the task for all simulations
    
    for (i in seq(length = nrow(c_par_factor)))
    {        
        i_factors <- c_com_factor
        for (j in seq(along = par_factor))
        {
            i_factors[[par_factor[j]]] <- c_par_factor[[i, par_factor[j]]]
        }
        v_names <- names(i_factors)
        sim_title <- as.character(apply(i_factors, 1, FUN = function(x)
            {
                x <- gsub(' ', '', x)
                # x <- gsub('/', '_', x)
                x <- paste(paste(v_names, x, sep = '='), collapse = ',')
                return(x)
            }))
        file_prefix <- unlist(lapply(sim_title, digest, algo = 'md5'))
        
        mysql <- as.data.frame(matrix(NA, nrow = nrow(i_factors), ncol = 0))
        mysql$id <- seq(along = file_prefix) + ids
        ids <- max(mysql$id)
        
        input_name <- i_factors[,input_factor]
        if (length(input_factor) > 1)
        {
            input_name <-  apply(i_factors[,input_factor], 1, paste, collapse = '_')
        }
        mysql$inputs <- sprintf('%s\\%s\\Input\\Sims\\%s.Rds', sharedrive, project, input_name)
        
        mysql$commands <- paste(r_script,  ' ', r_code, ' ',
            sim_title, ' ', file_prefix, ' 1>nul 2>nul', sep = '')
        
        if (is.null(folder_prefix))
        {
            i_folder_output <- paste(c_par_factor[i,], collapse = '_')
        } else
        {
            i_folder_output <- paste0(folder_prefix, '/',
                paste(c_par_factor[i,], collapse = '_'))
        }
        output_dir <- sprintf('%s\\%s\\Output\\%s', sharedrive, project, i_folder_output)
        
        mysql$outputs <- paste(output_dir, '\\',
            file_prefix, '.Rds', sep = '')
        
        con <- connectTaskDB()
        addTasks(con, project, mysql)
        disconnectTaskDB(con)
        
        output_files <- i_factors
        output_files$prefix <- file_prefix
        output_files$outputs <- paste0(i_folder_output, '/', file_prefix, '.Rds')
        
        save(output_files, mysql, file = file.path(local, project, 'Factor',
            sprintf('%s.RData', i_folder_output)), compress = TRUE)
    }
    
    # Create output folder
    folder_outputs <- apply(c_par_factor, 1, paste, collapse = '_')
    if (!is.null(folder_prefix))
    {
        folder_outputs <- paste0(folder_prefix, '/', folder_outputs)
    }
    lapply(paste0(local, '/', project, '/Output/', 
        folder_outputs), dir.create, showWarnings = FALSE)
    ids
}



#' Post-processing APSIM outputs into netcdf files
#'
#' @param filename The filename of netcdf file
#' @param years The years in the APSIM and netcdf 
#' @param factors The factors in the netcdf and APSIM
#' @param traits The vector of traits write into netcdf file
#' @param par_factor The factors will be parallel processed
#' @param base The base folder to store APSIM output files
#' @param factor_files The data.frame or vector of files to specify the output file names and levels
#' @param base_sub The sub-folder to store the single output files
#' @param merge_out_nc Whether merge the parallel outputs into an array when updating netcdf files.
#' @param yearvar Variable name of yeas in the APSIM output file
#' @param cpu_num The cpu number of parallel
#' @param tmp_folder The tmp folder to store merge APSIM output files
#' @export
apsimOut2Nc <- function(filename, years, factors, traits, par_factor,
    base, factor_files, 
    base_sub = NULL,
    merge_out_nc = TRUE,
    yearvar = 'year',
    cpu_num = 14, 
    tmp_folder = file.path('Results', 'tmpNc')
    )
{
    library(RAPSIM)
    library(ncdf4cf)
    library(ncdf4)
    # print('Create a empty netcdf file.')
    ncCreateApsim(filename, years, factors, traits)
    
    par_factor <- names(factors[(names(factors) %in% par_factor)])
    
    #print('Create the temp folder to store the merge RData files of APSIM.')
    # Check whether tmp folder existing
    if (!file.exists(tmp_folder))
    {
        dir.create(tmp_folder)
    }
    
    mergeApsimOut <- function(i, par_grid, filename, 
        factors, years, traits, factor_files, base,
        yearvar, tmp_folder, base_sub)
    {
        output <- file.path(tmp_folder, 
            paste0(paste(par_grid[i,], collapse = '_'), '.RData'))
        if (!file.exists(output))
        {
            library(RAPSIM)
            library(ncdf4)
            library(digest)
            if (is.character(factor_files))
            {
                if (length(factor_files) > 1)
                {
                    vars <- load(factor_files[grep(paste(par_grid[i,], collapse = '_'), factor_files)])
                } else
                {
                    load(factor_files)
                }
            }
            
            pos <- rep(TRUE, nrow(output_files))
            par_f_names <- names(par_grid)
            for (j in seq(along = par_f_names))
            {
                pos <- pos & output_files[[par_f_names[j]]] %in% par_grid[i,j]
            }
            
            if (!is.null(output_files$outputs))
            {
                files <- paste0(base, '/Output/',
                                output_files$outputs)
            } else 
            {
                if (is.null(base_sub))
                {
                    files <- paste0(base, '/Output/',
                                paste(par_grid[i,], collapse = '_'), '/',
                                output_files$prefix[pos], '.RData')
                } else
                {
                    files <- paste0(base, '/Output/',
                                base_sub, '/',
                                output_files$prefix[pos], '.RData')
                
                }
            }
            # sum(!file.exists(files))
            nc <- nc_open(filename)    
            args <- c(list(nc), as.list(unlist(par_grid[i,])))
            names(args) <- c('nc', names(par_grid))
            
            start <- do.call(ncGetStart, args)
            
            dims <- c(length(years), as.numeric(unlist(lapply(factors, length))))
            dims[match(names(par_grid), names(factors)) + 1] <- 1
            
            res.array <- array(NA, dim = dims)
            
            outputs <- ncReadApsimOut(nc, files, start,
                                      res.array, 
                                      yearvar = yearvar, 
                                      traits = traits)
            save(outputs, file = output, compress = TRUE)
            nc_close(nc)
            return(invisible())
        }
    }
    
    #print('Merge the single APSIM output files into a bigger RData files.')
    # par_factor <- names(factors)[names(factors) %in% par_factor]
    par_grid <- expand.grid(factors[par_factor], stringsAsFactors = FALSE)

    library(snowfall)
    sfInit(parallel = TRUE, cpus = cpu_num)
    sfLapply(seq(length = nrow(par_grid)), 
        mergeApsimOut, par_grid, filename, 
        factors, years, traits, factor_files, base,
        yearvar, tmp_folder, base_sub)
    sfStop()

    #print('Write RData files into netcdf file.')
    if (merge_out_nc)
    {
        dims <- c(length(years), as.numeric(unlist(lapply(factors, length))))
                
        res.array <- array(NA, dim = dims)
        s_outputs <- list()
        s_outputs$start <- rep(1, length(factors) + 1)
        s_outputs$count <- dims
        for (j in seq(along = traits))
        {
            s_outputs[[traits[j]]] <- res.array
        }

        for (i in seq(nrow(par_grid)))
        {
            load(file = file.path(tmp_folder, 
                paste0(paste(par_grid[i,], collapse = '_'), '.RData')))
            for (k in seq(along = traits))
            {
                FUN <- function(x) NULL
                empty_arg <- formals(FUN)
                dim_list <- replicate(length(dims), unname(empty_arg))
                for (j in seq(ncol(par_grid)))
                {
                    pos <- match(names(par_grid[j]), names(factors)) + 1
                    dim_list[pos] <- outputs$start[pos]
                }
                s_outputs[[traits[k]]] <- do.call(`[<-`, c(list(s_outputs[[traits[k]]]), dim_list, list(outputs[[traits[k]]])))
            }
        }

        nc <- nc_open(filename, write=TRUE)
        ncUpdateApsim(nc, outputs = s_outputs)
        # nc_sync(nc)
        nc_close(nc)
    } else
    {
        nc <- nc_open(filename, write=TRUE)
        par_grid <- expand.grid(factors[par_factor], stringsAsFactors = FALSE)
        for (i in seq(nrow(par_grid)))
        {
            load(file = file.path(tmp_folder, 
                paste0(paste(par_grid[i,], collapse = '_'), '.RData')))
            ncUpdateApsim(nc, outputs = outputs)
            nc_sync(nc)
        }
        nc_close(nc)
    }
}



