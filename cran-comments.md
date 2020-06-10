## Test environments
* local R installation, R 4.0.0
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel)

## R CMD check results

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Adam Fleischhacker <ajf@udel.edu>'
  
  New submission

0 errors √ | 0 warnings √ | 1 note x


## rhub::check_for_cran() 

Runs without error.


## My Respone to Comments from Jelena Saf (CRAN) on causact 0.2.2 submission

Thank you for taking the time to review the package and for making very concise and descriptive recommendations.  I have a addressed them all as documented below:

* Please shorten the title to a maximum of 65 characters.
    - RESPONSE:  Title shortened to 40 characters - "Accelerated Bayesian Analytics with DAGs"


* Acronyms/Abbreviations can be used on their own in the title as long as they are explained in the description field.
    - RESPONSE:  The DAG acronym in the title is described in the description field.

* Please always write non-English usage, package names, software names and API names in *undirected* single quotes in title and description in the DESCRIPTION file. e.g. --> 'R'
    - RESPONSE:  Fixed.  Thx.

* \dontrun{} should be only used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user.
Please unwrap the examples if they are executable in < 5 sec, or create additionally small toy examples to allow automatic testing.
    - RESPONSE:  Unwrapped all examples and wrapped only the parts which require TensorFlow installation or are sufficiently complex that they have been taking around 5 to 6 seconds.  For all long examples that are not run, a shorter example exists.

* Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means.
(If a function does not return a value, please document that too, e.g. 
\value{No return value, called for side effects} or similar)
    - RESPONSE: All exported functions now have this description.  I also stopped exporting a couple of internal functions that were mistakenly tagged.

* You write information messages to the console that cannot be easily suppressed.  It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object.  Instead of print()/cat() rather use message()/warning()  or
if(verbose)cat(..) if you really have to write text to the console.
(except for print, summary, interactive functions)
    - RESPONSE:  The `dag_greta` function was the offender.  It now outputs a message to the console instead of printing to the console when argument `mcmc=FALSE` (not the default).  Users can now assign output of this function to their own object instead of the global environment being changed.     

* Please make sure you are not modifying the global environment.
    - RESPONSE: Fixed. The `dag_greta` function did this, but no longer does.  Instead of modifying the global environment, it outputs a data frame and stores additional information for subsequent retrieval in an environment called `cacheEnv`.  Documentation also updated accordingly. 

* You also seem to be a copyright holder [cph]. Please add this information to the Authors@R field.
    - RESPONSE: Fixed.

* Please fix and resubmit, and document what was changed in the submission comments. Best, Jelena Saf
    - RESPONSE: Many thanks for the help.  This resubmission addresses all comments.

