# SCAdmin

## Adobe Analytics Admin and Segments API functions

Version 0.0.0.9000 (in dev)

**Functions to access more advanced Adobe Analytics admin API methods, as well as in-depth
Segments API methods.**

At the moment, this package combines both Admin and Segments functions, but this is subject to 
change, and the package may be split. The Admin API methods this package will ultimately 
address require full admin privileges, while the overwhelming
majority of potential Segments methods end-users neither have, nor require, access privileges
beyond the standard level (i.e. non-admin). 

Also note that the Segments methods, or at least the `GET` Segments methods, are targeted for 
integration with the official [RSiteCatalyst package](https://github.com/randyzwitch/RSiteCatalyst)
package. This is for the good of all current and future end-users (and developers/maintainers).


**Quick Nav**  

[How to install](#how-to-install)  
[Key dependencies](#key-dependencies)  
[Package structure](#package-structure)  


### How to install

There are several ways to get and install this package:

#### From source

This is the most manual approach, but does not require persistent access to the repository. All you 
need is the tarball (tar.gz; see below). While obtaining said tarball requires repository access, 
anyone with the tarball can install the package using the instructions in the code chunk thereafter.

1. Navigate to the master branch (https://git.cm-elsevier.com/zhang1/SCAdmin/tree/master)
2. Download the tar.gz (even if you are running Windows)
3. Open an R session; probably most convenient to open the session in the file download location.  
    
Modify the following code chunk:  

```{r}
##The basic pattern is: 'install.packages(path_to_file, repos = NULL, type="source")'
##For example:
# install.packages("C:/Users/zhang2/Downloads/SCAdmin-master-37c6596be1b7afb2d65e0ecabcca9f87aa8812e6.tar.gz", 
#                  repos = NULL, 
#                  type="source") #Modify the path for your local system
```

#### Clone

This requires access to the repository, and you will need to either provide credentials, or pre-configure your
git environment to authenticate. 

```{shell}
#In a git bash shell, type:
git clone https://git.cm-elsevier.com/zhang1/SCAdmin.git
```  

Once you've cloned the repo, you can install it using devtools:

```{r}
##Open a fresh RStudio session (anywhere)
##Do not have any libraries loaded!

##Uncomment the lines below if necessary to install the required packages
# install.packages("devtools") #if you do not have it already

##Then use devtools::install() to install the package, e.g:
##devtools::install("path_to_cloned_project")
# devtools::install("C:/Users/zhang2/Desktop/zhang1GitLab/SCAdmin") #modify the path for your system

##NOTE: Do not include the trailing slash at the end of the package dir, i.e. do "SCAdmin" and NOT "SCAdmin/"
```
And once you've done the initial clone, you do not need to clone the repo again for updates (although you could). Instead, 
you can simply do a pull or fetch + merge:

```{shell}
#In a git bash shell, type:
git fetch origin master
git merge origin master

# Or, you can just use
git pull origin master

#You may need to authenticate using your gitlab user name and password each time, unless you've
# pre-configured git to save your credentials. 
```

#### Direct pull

This is the easiest method, as it can be performed within R itself. This method requires you to supply
your gitlab user name and password, which can be done in any number of ways. In the example, I've stored
my personal gitlab credentials in a `.Renviron` file; see 
[relevant instructions in Authentication](#a-bit-more-difficult-but-recommended-renviron).

```{r}
##Open a fresh RStudio session (anywhere)
##Do not have any libraries loaded!

##Uncomment the lines below if necessary to install the required packages
# install.packages("git2r") #if you do not have it already
# install.packages("devtools") #if you do not have it already

library(git2r)

##First, store your gitlab credentials in a variable called 'my_gl_cred'
##Note that in this example, I've stored this information in a .Renviron file. See
## Key dependencies --> Authentication for information on using .Renviron to store
## credientials, specifically why one would use this approach, and the pros and cons.

##You will need to replace the 'wz_gl_id' and 'wz_gl_pw' variable names with whatever
## names you use with your .Renviron file.
my_gl_cred <- cred_user_pass(Sys.getenv("wz_gl_id"), password = Sys.getenv("wz_gl_pw"))

##Then, call install via devtools; pass through the 'my_gl_cred' variable to authenticate
devtools::install_git("https://git.cm-elsevier.com/zhang1/SCAdmin.git", 
                      credentials = my_gl_cred)
```
### Key dependencies

#### Authentication (VERY IMPORTANT)

**_This is the most important part of getting started_** (in case you missed the emphasis above)

**You must not only have access to Adobe Analytics Web Services, but also appropriate administrative privileges
for the suite(s) you wish to manage, and in most cases, you will not be able to use any of the `PUSH` methods without
full admin rights**. Contact your current Adobe Analytics administrator for questions/requests.  

The [RSiteCatalyst package](https://github.com/randyzwitch/RSiteCatalyst) provides foundational capabilities that this
package reuses. This includes the authentication method within RSiteCatalyst, `SCAuth()`. At the moment, Elsevier uses
the _legacy_ method, which requires a key and secret. These are, respectively, your **Web Services** User Name and 
Shared Secret. 

##### Easy, but not recommended (literal plain text pass-through)
The most straightforward way to authenticate, within R, via `RSiteCatalyst::SCAuth()`, is to simply copy and paste
your User Name and Shared Secret directly into the function, like so:

```{r}
##load the library; should already be installed if you install this package
library(RSiteCatalyst) 
SCAuth(key = "YOUR_WEBSERVICES_USER_NAME", secret = "YOUR_WEBSERVICES_SHARED_SECRET")
```
RSiteCatalyst takes care of the rest. 

##### A bit more difficult, but recommended (.Renviron)
The above authentication method is not recommended if you plan to share code, and particularly not recommended if you
plan to use a remote version control system (VCS) such as github or gitlab, as you will check-in authentication information.
Additionally, you will need to keep track of your authentication credentials each time you start a new project. 

A better method is to use a `.Renviron` file. Don't be scared-- it's much less difficult than it sounds. 

**In a fresh RStudio session**:
1. First, locate your home dir, as this is where you will save the .Renviron file you are going to create:  
    `normalizePath("~/")`
    - On Windows, this might be more informative/easier to read:  
    `Sys.getenv("R_USER")`

2. Then, in RStudio, navigate to File --> New File --> Text file
    - The format of this should be e.g. (include the quotes):
        - `wz_sc_id = "YOUR_WEBSERVICES_USER_NAME"`
        - `wz_sc_pw = "YOUR_WEBSERVICES_SHARED_SECRET"` `<press ENTER>` (must end this file in a blank newline)
    - You should have **three lines where the third (last) line is blank**, IF this is a brand-new .Renviron file. 
    If not, you should have one more line than the total number of entries in your .Renviron file, assuming you do 
    not have any spaces between .Renviron entries. In any event, the very last line of your .Renviron file MUST
    be a blank newline (`\n`).
        - You should use names that you can easily remember, as you will use these 
        names (keys) to populate parameters (values) in the call to `SCAuth()`

3. Then, save the file as `.Renviron` in the location denoted by Step #1. 
    - Your OS may complain/warn about file extensions. Ingore these and proceed. 
    - Your final file must be literlly named `.Renviron`.

4. Restart your R session; the default keyboard shortcut is `<CTRL + SHIFT + F10>` 

5. You can now access the credentials in key-value fashion using `Sys.getenv()`, 
    - e.g. if you have saved your USER_NAME in a variable called "sc_id", with a value of "f.lastname:Elsevier Inc.",
    then the command `Sys.getenv("sc_id")` should return `"f.lastname:Elsevier Inc."`

```{r}
##For example:  
library(RSiteCatalyst)  
##You would insert your own .Renviron variable values in the line below:
SCAuth(key = Sys.getenv("wz_sc_id"), secret = Sys.getenv("wz_sc_pw") 
```
This method requires a bit more upfront work, but lets you use the same authentication mechanism on your local
machine for any project/script, and will persist across RStudio and R version upgrades. 

You will need to do this on each machine you plan to use, but again, it's a one-time effort. 

To reiterate, this means you can share and check code into VCS without fear of persisting credentials. 
This method is, however, **not** strictly secure, as you are still storing credentials in plain text. It's
a bit of security by anonymity, but should anyone access your local system, and know where to look, it's 
entirely possible they can access your Adobe Analytics credentials. 

If you require more robust password management solutions, there are packages that manage this, although I've not
investigated yet. It's on my to-do list and a pull request would be welcome if you wish to contribute. 


### Package structure

#### High-level organization

When complete, this package should provide, at minimum, the following functions for end-users:

**Admin API Methods**

- [ ] Get_Group (`Permissions.GetGroup`)
- [ ] Get_Logins (`Permissions.GetLogins`)
- [ ] Get_Login (`Permissions.GetLogin`)
- [ ] Get_ReportSuiteGroups (`Permissions.GetReportSuiteGroups`)

*Additional Admin API methods e.g. `PUT/EDIT/DELETE` are being considered, but not prioritized.*

**Segments API Methods**

- [ ] Get_Segments (`Segments.Get`)
- [ ] Save_Segment (`Segments.Save`)
- [ ] Edit_Segment (`Segments.Save`)
- [ ] Delete_Segment (`Segments.Delete`)

Please note additionally:

- For both Admin and Segments API methods, `GET` methods are being prioritized for development. 
- Additional functions will also be provided for `PUT/EDIT/DELETE` methods. These will e.g. assist in 
  creation of appropriate data structures. The nomenclature of such functions will be different from 
  what is outlined below.
  
#### General naming conventions

**GET functions**

All `GET` functions are broken into two primary parts. In all cases, this means at least two `R` script files
within the `/R` directory:

- A `call.<some_name>.R` function (makes the call)
- A `restr.<some_name>.R` function (restructures/parses the return)

For particularly complex `restr.` requirements, there may be an additional standalone `R` script named as 
follows (*This is a temporary organization decision during development;*
*will have a single .restr per complete function for release*):

> `restr_` (underscore rather than dot delimiter)

Ultimately, each pair of `call.` and `restr.` functions will be combined in a simple wrapper to create the 
final end-user facing function. At minimum, it is expected that the underlying `call.` function will 
be exported, along with the end-user facing function. This provides an escape hatch for unanticipated
scenarios where a call return cannot be parsed by the package `restr.` function, so that users are not 
stuck with a completely ineffective function, and makes debugging easier.

Using the `Permissions.GetGroup` method within the Admin API as an example:

1. Craft a call to the method, with input checks and error handling
    - This is handled by `call.Get_Group()`
2. Restructure the call return, taking into account the various possible returns, depending on
   supplied parameters. 
    - This is handled by `restr.Get_Group()`
    - Additionally, the return if a user requests `permissions` is handled
      by `restr_permissions.R`, as such returns are a bit more complex.
3. Create a simple wrapper function that strings the above together
    - This would be handled by a end user-facing function called e.g. `Get_Group()`
    - As of 2017-02-01, `Get_Group()` does not exist; writing tests for 
      underlying call and restr functions first.

#### R File organization

With the single exception of `helpers_global.R`, every file within `/R` describes a single function that is 
suitable for export. This does not mean that they *must* be exported, although this is presently the case.

These functions are referred to as `core functions` henceforth, for convenience and clarity. 

#### Helper functions

Helper functions are never exported; *helpers* are classified as such on the basis of their specificity and/or
their trivial nature. In other words, they are either poorly generalizable and/or they perform trivial tasks.  

There are two types of helper functions:

- Those within `helpers_global.R`
    - Always prefixed with `.g_helper_`
    - Generally applicable (across core functions)
- Those within core functions
    - Always prefixed with `.l_helper_`
    - Applicable only within the context of the respective core function


### Internal functions

At the moment, there is only a single function with with the `@internal` keyword. The anticipated use
case is for Admin API methods



### Note to future developers
