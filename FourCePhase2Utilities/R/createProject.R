#' Creates repository infrastructure for a 4CE Phase 2 Project.  Creates local repositories,
#' prompts for a GitHub username, and tries to create the corresponding GitHub repositories
#' and push inital contents.
#'
#' @param projectName The name of the project.  Will be used to create correlated repository names.
#' @param workingDirectory The directory in which the project repositories will be created.
#' 
#' @keywords 4CE Phase2 Project
#' @export

createProject <- function (projectName, workingDirectory="/RDevelopment") {

    ## names for the directories to be created
    rPackageRespositoryName = paste(
        sep = "", 
        "Phase2",
        projectName,
        "RPackage"
    )

    siteDataRepositoryName = paste(
        sep = "", 
        "Phase2",
        projectName,
        "ResultsPerSite"
    )

    countryDataRepositoryName = paste(
        sep = "", 
        "Phase2",
        projectName,
        "AggregateResultsPerCountry"
    )

    ## R package repository
    rPackageParentRepositoryPath = 
        file.path(
            workingDirectory, 
            rPackageRespositoryName
        )

    ## site data repository
    siteDataRepositoryPath = 
        file.path(
            workingDirectory, 
            siteDataRepositoryName
        )

    ## country data repository
    countryDataRepositoryPath = 
        file.path(
            workingDirectory, 
            countryDataRepositoryName
        )

    if (dir.exists(rPackageParentRepositoryPath)) {
        stop(rPackageParentRepositoryPath, " already exists!")
    }   
    
    if (dir.exists(siteDataRepositoryPath)) {
        stop(siteDataRepositoryPath, " already exists!")
    }
    
    if (dir.exists(countryDataRepositoryPath)) {
        stop(countryDataRepositoryPath, " already exists!")
    }

    ## create the directories for the repositories
    dir.create(rPackageParentRepositoryPath)
    dir.create(siteDataRepositoryPath)
    dir.create(countryDataRepositoryPath)

    ## add READMEs
    writeReadmeMd(
        projectName = rPackageRespositoryName, 
        workingDirectory = rPackageParentRepositoryPath, 
        message = paste(
            sep="", 
            "R code to run, validate, and submit the analysis for the ", projectName, " project.\n\n",
            "To install this package in R:\n\n",
            "```\n",
            "devtools::install_github(\"https://github.com/covidclinical/", 
                rPackageRespositoryName, 
                "\", subdir=\"", 
                "FourCePhase2",
                projectName,
                "\", upgrade=FALSE)\n",
            "```\n")
    )

    writeReadmeMd(
        projectName = siteDataRepositoryName, 
        workingDirectory = siteDataRepositoryPath, 
        message = paste(sep="", "Site data repository for the ", projectName, " project.")
    )

    writeReadmeMd(
        projectName = countryDataRepositoryName, 
        workingDirectory = countryDataRepositoryPath, 
        message = paste(sep="", "Country data repository for the ", projectName, " project.")
    )

    ## directory in which the R package will be built under the Git repository
    rPackagePath = file.path(
        rPackageParentRepositoryPath,
        paste(
            sep = "",
            "FourCe", 
            "Phase2",
            projectName
        )
    )

    ## create the R package inside of that directory
    usethis::create_package(
        path = rPackagePath
    )

    ## create the three required stubs for the R package
    createPhase2Stubs(projectName, rPackagePath, siteDataRepositoryName, countryDataRepositoryName)

    writeLines("You will be prompted for your GitHub username and password a total of 6 times.\n")

    ## initialize git repositories
    doInitializeAddCommit(rPackageParentRepositoryPath)
    doInitializeAddCommit(siteDataRepositoryPath)
    doInitializeAddCommit(countryDataRepositoryPath)

    ## create GitHub repositories and push
    createGitHubRepositoryAndPush(rPackageRespositoryName, rPackageParentRepositoryPath)
    createGitHubRepositoryAndPush(siteDataRepositoryName, siteDataRepositoryPath)
    createGitHubRepositoryAndPush(countryDataRepositoryName, countryDataRepositoryPath)
}

#' Returns the text of the empty R function call stub for the project 
#'
#' @param projectName The name of the project.
#' @param functionName The name of the function to be emitted.
#' @param commentPreamble The preamble of the comments to be emitted.
#' @param functionBody Any text to be included in the function body.

emit4CeFunctionStub <- function (projectName, functionName, commentPreamble, functionBody="") {
    return(
        paste(
            sep="",
            "
#' ", commentPreamble, " for the ", projectName, " project
#'
#' @keywords 4CE Phase2 Project
#' @export

", functionName ," <- function() {
", functionBody, "
}
"
        )
    )
}

#' Writes the result of calling emit4CeFunctionStub to a file named functionName.R
#'
#' @param projectName The name of the project.
#' @param functionName The name of the function to be emitted.
#' @param commentPreamble The preamble of the comments to be emitted.
#' @param workingDirectory The working directory into which the file will be written
#' @param functionBody Any text to be included in the function body.

emit4CeFunctionStubToFile <- function (projectName, functionName, commentPreamble, workingDirectory, functionBody="") {

    writeLines(
        emit4CeFunctionStub(
            projectName=projectName, 
            functionName=functionName, 
            commentPreamble=commentPreamble,
            functionBody=functionBody
        ), 
        con=file.path(workingDirectory, "R", paste(sep="", functionName, ".R"))
    )
}

#' Creates R function stubs for a 4CE Phase 2 Project 
#'
#' @param projectName The name of the project.
#' @param workingDirectory The directory in which the code stubs will be written.
#' @param siteDataRepositoryName The name of the repository for the site data for this project.
#' @param countryDataRepositoryName The name of the repository for the country data for this project.

createPhase2Stubs <- function (projectName, workingDirectory, siteDataRepositoryName, countryDataRepositoryName) {

    ## runAnalysis()
    emit4CeFunctionStubToFile(
        projectName=projectName, 
        functionName="runAnalysis", 
        workingDirectory=workingDirectory,
        commentPreamble="Runs the analytic workflow",
        functionBody="\t#TODO: implement analysis"
    )

    ## validateAnalysis()
    emit4CeFunctionStubToFile(
        projectName=projectName, 
        functionName="validateAnalysis", 
        workingDirectory=workingDirectory,
        commentPreamble="Validates the results of the analytic workflow",
        functionBody="\t#TODO: implement validation"
    )

    ## TODO: add code to push result files to github; may require too much bookkeeping w.r.t. generated files
    ## submitAnalysis()
    emit4CeFunctionStubToFile(
        projectName=projectName, 
        functionName="submitAnalysis",
        workingDirectory=workingDirectory, 
        commentPreamble="Submits the results of the analytic workflow",
        functionBody="\t#TODO: implement data submission"
    )

    ## submitAnalysis()
    emit4CeFunctionStubToFile(
        projectName=projectName, 
        functionName="submitAnalysis",
        workingDirectory=workingDirectory, 
        commentPreamble="Submits the results of the analytic workflow",
        functionBody="\t#TODO: implement data submission"
    )

    ## getSiteDataRepositoryUrl()
    emit4CeFunctionStubToFile(
        projectName=projectName, 
        functionName="getSiteDataRepositoryUrl",
        workingDirectory=workingDirectory, 
        commentPreamble="Returns the GitHub URL of the Site Data Repository for this project",
        functionBody=paste(sep="", "\treturn(\"https://github.com/covidclinical/", siteDataRepositoryName, ".git\")")
    )

    ## getSiteDataRepositoryUrl()
    emit4CeFunctionStubToFile(
        projectName=projectName, 
        functionName="getAggregateCountryDataRepositoryUrl",
        workingDirectory=workingDirectory, 
        commentPreamble="Returns the GitHub URL of the Site Data Repository for this project",
        functionBody=paste(sep="", "\treturn(\"https://github.com/covidclinical/", countryDataRepositoryName, ".git\")")
    )
}

#' Writes a placeholder README.md for a git repository
#'
#' @param projectName The name of the git repository
#' @param workingDirectory The directory in which the README.md will be written.

writeReadmeMd <- function(projectName, workingDirectory, message="") {

    writeLines(
        con=file.path(workingDirectory, "README.md"),
        paste(sep="",
"# ", projectName, "
", message, "
"
        )
    )
}


#' Given a path to a directory, this function initialzies a new Git repository there,
#' then adds all of the current contents and does an initial commit.
#'
#' @param repositoryPath The path to the directory that will become the git repository

doInitializeAddCommit <- function(repositoryPath) {
    
    ## init the repository
    system(
        paste(
            sep = "",
            "git init ", repositoryPath
        )
    )

    ## save current directory and change to the specified working directory
    originalDirectory = getwd()
    setwd(repositoryPath)

    ## add the current directory contents and run an initial commit
    system(
        paste(sep="", "git add . ", repositoryPath, "; git -c user.email=\"4CE@i2b2transmart.org\" -c user.name=\"4CE Consortium\" commit -m \"auto generated initial commit\"")
    )

    ## return to the directory we were in when we started
    setwd(originalDirectory)
}

#' Given a path to a directory that contians a local Git repository,
#' this function uses the GitHub web API to create a new repository under
#' the covidclinical organization, adds it as a remote to the local repository,
#' and pushes to the new GitHub remote.
#'
#' @param repositoryName The name of the repository
#' @param repositoryPath The path to the directory that contains the local git repository

createGitHubRepositoryAndPush <- function(repositoryName, repositoryPath) {

    ## prompt for github username
    username = readline("GitHub username: ")

    ## use GitHub API to create the remote
    system(
        paste(
            sep="",
            "curl -u '", username, "' https://api.github.com/orgs/covidclinical/repos -d '{\"name\":\"", repositoryName, "\"}'"
        )
    )

    ## add the remote
    originalDirectory = getwd()
    setwd(repositoryPath)
    system(
        paste(
            sep="",
            "git remote add origin https://github.com/covidclinical/", repositoryName, ".git"
        )
    )

    ## push
    system("git push -u origin master")
    
    ## return to the directory we were in when we started
    setwd(originalDirectory)
}