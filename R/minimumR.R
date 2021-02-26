# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   11:02 AM Wednesday, 13 March 2013
# * Copyright: AS IS
# *

#' Prepare minimum R through only copying necessary files.
#'
#' @param pkgs A character vector of export packages
#' @param output The output folder of all files 
#' @param r.home Home directory of R
#' @param arch The architecture of R will be copied, i368 or x64. NO implement now.
#' @export
minimumR <- function(pkgs, output = 'MR', r.home = R.home(), 
    arch = 'i386')
{
    base_files <- c('bin/i386/R.dll', 
        'bin/i386/Rblas.dll',
        'bin/i386/Rgraphapp.dll',
        'bin/i386/Riconv.dll',
        'bin/i386/Rlapack.dll',
        'bin/i386/Rscript.exe',
        'bin/i386/Rzlib.dll',
        'modules/i386/lapack.dll',
        'etc/Rconsole')
    base_files <- gsub('i386', arch, base_files)
    # Clean and create output folders
    if (file.exists(output))
    {
#         file.remove(list.files(output, '*.*', recursive = TRUE, 
#             full.names = TRUE, include.dirs = TRUE))
    } else 
    {
        dir.create(file.path(output))
    }

    recursiveCopy <- function(source, target, files)
    {
        for (i in seq(along = files))
        {
            dir_path <- dirname(file.path(target, files[i]))
            if (!file.exists(dir_path)) {
                dir.create(dir_path,
                       recursive = TRUE)
            }
            file.copy(file.path(source, files[i]), 
                      file.path(target, files[i]))
        }
    }
    recursiveCopy(r.home, output, base_files)
    # Copy packages

    lib_root <- file.path(output, 'library')
    dir.create(lib_root, showWarnings = FALSE)
    for (i in seq(along = pkgs))
    {
        lib_path <- file.path(.libPaths(), pkgs[i])
        lib_path <- lib_path[file.exists(lib_path)]
        if (length(lib_path) == 0)
        {
            stop(sprintf('Package "%s" doesn\'s exist.', pkgs[i]))
        }
        lib_files <- list.files(lib_path, '*.*', full.names = TRUE, recursive = TRUE)
        lib_files <- lib_files[-grep('/help/|/html/|/doc/|/po/|/tests/|/Sweave/|/misc/|/data/|/examples/|/demo/|/exampleData/|/scripts/', lib_files)]
        pos <- grep(ifelse(arch == 'i386', 'x64', 'i386'), lib_files)
        if (length(pos) > 0)
        {
            lib_files <- lib_files[-grep(ifelse(arch == 'i386', 'x64', 'i386'), lib_files)]
        }
        
        if (length(grep('Program Files', lib_path)) > 0)
        {
            lib_files <- gsub('.*/library/(.*)', '\\1', lib_files)
            lib_base <- file.path(r.home, 'library')
        } else
        {
            lib_files <- file.path(pkgs[i], gsub(sprintf('%s/(.*)', lib_path), '\\1', lib_files))
            lib_base <- gsub(sprintf('(.*/)%s', pkgs[i]), '\\1', lib_path)
        }
        recursiveCopy(lib_base, lib_root, lib_files)
    }
    return(invisible())
}
