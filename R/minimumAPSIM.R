# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:19 PM Wednesday, 07 August 2013
# * Copyright: AS IS
# *


#' Create the minimum requirements to run an APSIM simulation (APSIM 7.5)
#'
#' @param template The sim file as the template
#' @param apsim The installed path of APSIM
#' @param output The output folder of all files 
#' @export
minimumAPSIM <- function(template, apsim, output = 'APSIM75')
{
    dir.create(output)
    dir.create(file.path(output, 'model'))
    sim <- readLines(template)
    # Find the modules used in the sim file
    pos <- grep('(\\\\|/)Model(\\\\|/).*\\.dll', sim)
    dlls <- unique(gsub('^.*(\\\\|/)Model(\\\\|/)(.*\\.dll).*$',
        '\\3', sim[pos]))
    
    base_files <- c('Protocol.dll', 'ProtocolManager.dll',
        'Accum.dll', 'ApsimFile.dll', 'ApsimModel.exe',
        'ApsimShared.dll', 'ComponentInterface.dll', 'ComponentInterface2.dll',
        'CSGeneral.dll', 'FortranComponentInterface.dll', 'FortranComponentInterface2.dll',
        'General.dll', 'iconv.dll', 'libxml2.dll')
    dlls <- unique(c(base_files, dlls))
    dlls <- file.path(apsim, 'model', dlls) 
    # Copy the APSIM files
    file.copy(dlls, file.path(output, 'model', basename(dlls)))
    file.copy(file.path(apsim, 'Apsim.xml'), file.path(output, 'Apsim.xml'))
    if (file.exists('C:/Windows/SysWOW64/msvcp100.dll'))
    {
        file.copy('C:/Windows/SysWOW64/msvcp100.dll', 
            file.path(output, 'model', 'msvcp100.dll'))
    } else
    {
        warning('Cannot find C:/Windows/SysWOW64/msvcp100.dll. You have to manually copy it to model folder')
    }
    if (file.exists('C:/Windows/SysWOW64/msvcr100.dll'))
    {
        file.copy('C:/Windows/SysWOW64/msvcr100.dll', 
            file.path(output, 'model', 'msvcr100.dll'))
    } else
    {
        warning('Cannot find C:/Windows/SysWOW64/msvcr100.dll. You have to manually copy it to model folder')
    }
    return(invisible())
}
