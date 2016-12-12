# SCAdmin

## Adobe Analytics Admin API functions

Version 0.0.0.9000 (in dev)

### How to install

There are several ways to get and install this package:

#### From source

This is the most manual approach, but does not require actual access to the repository. All you 
need is the tarball (tar.gz; see below). Getting said tarball requires repository access, but
this method does allow anyone with the tarball to pass the file to others, and install the package.

1. Navigate to the master branch (https://git.cm-elsevier.com/zhang1/SCAdmin/tree/master)
2. Download the tar.gz (even if you are running Windows)
3. Open an R session; probably most convenient to open the session in the file download location.  
    
Modify the following code chunk:  

```{r}
# install.packages(path_to_file, repos = NULL, type="source")
#For example:
# dontrun{
# install.packages("path_to_package/SCAdmin-master-37c6596be1b7afb2d65e0ecabcca9f87aa8812e6.tar.gz", 
#                  repos = NULL, 
#                  type="source")
# }
```

#### Clone

This requires access to the repository, and you will need to either provide credentials, or pre-configure your
git environment to authenticate. 

```{r}
#In a git bash shell, type:
# dontrun{
# git clone https://git.cm-elsevier.com/zhang1/SCAdmin.git
# }
```  

Once you've cloned the repo, you can install it using devtools:

```{r}
##install devtools if you do not have it already
# install.packages("devtools")

##Then use devtools::install() to install the package, e.g:
##devtools::install("path_to_cloned_project")

# devtools::install("C:/Users/zhang2/Desktop/zhang1GitLab/SCAdmin") #modify the path for your system

##NOTE: Do not include the trailing slash at the end of the package dir, i.e. do "SCAdmin" and NOT "SCAdmin/"
```
And once you've done the initial clone, you do not need to clone the repo again for updates (although you could). Instead, 
you can simply do a pull or fetch + merge:

```{r}
#In a git bash shell, type:
# dontrun{
# git fetch origin master
# git merge origin master
}
#You may need to authenticate using your gitlab user name and password each time, unless you've
#pre-configured git to save your credentials. 
```

#### Direct pull

This is the easiest method, as it can be performed within R itself. Instructions forthcoming once I've figured out
how to do this without passing credentials via clear text. 



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

##### Easy, but not recommended
The most straightforward way to authenticate, within R, via `RSiteCatalyst::SCAuth()`, is to simply copy and paste
your User Name and Shared Secret directly into the function, like so:

```{r}
library(RSiteCatalyst) #load the library; should already be installed if you install this package
SCAuth(key = "YOUR_WEBSERVICES_USER_NAME", secret = "YOUR_WEBSERVICES_SHARED_SECRET")
```
RSiteCatalyst takes care of the rest. 

##### A bit more difficult, but recommended
The above authentication method is not recommended if you plan to share code, and particularly not recommended if you
plan to use a remote version control system (VCS) such as github or gitlab, as you will check-in authentication information.
Additionally, you will need to keep track of your authentication credentials each time you start a new project. 

A better method is to use a `.Renviron` file. Don't be scared-- it's much less difficult than it sounds. 

1. First, locate your home dir:  
    `normalizePath("~/")`
    - On Windows, this might be more informative/easier to read:  
    `Sys.getenv("R_USER")`

2. Then, in RStudio, navigate to File --> New File --> Text file
    - The format of this should be e.g. (include the quotes):
        - `wz_sc_id = "YOUR_WEBSERVICES_USER_NAME"`
        - `wz_sc_pw = "YOUR_WEBSERVICES_SHARED_SECRET"` `<press ENTER>` (must end this file in a blank newline)
    - You should have **three lines where the third (last) line is blank**
        - You should use names that you can easily remember, as you will use these 
        names (keys) to populate parameters (values) in the call to `SCAuth()`

3. Then, save the file as `.Renviron` in the location denoted by Step #1. 

4. Restart your R session \<CTRL + SHIFT + F10\>

5. You can now access the credentials in key-value fashion using `Sys.getenv()`, 
    - e.g. if you have saved your USER_NAME in a variable called "sc_id", with a value of "f.lastname:Elsevier Inc.",
    then the command `Sys.getenv("sc_id")` should return `"f.lastname:Elsevier Inc."`

```{r}
#For example:  
library(RSiteCatalyst)  
SCAuth(key = Sys.getenv("wz_sc_id"), secret = Sys.getenv("wz_sc_pw")  
```
Yes, it's a few more steps, but this will let you use the same authentication mechanism on your local
machine for any project/script, and will persist across RStudio and R version upgrades. 

You will need to do this on each machine you plan to use, but again, it's a one-time effort. 

To reiterate, this means you can share and check code into VCS without fear of persisting credentials. 
This method is, however, **not** strictly secure, as you are still storing credentials in plain text. It's
a bit of security by anonymity, but should anyone access your local system, and know where to look, it's 
entirely possible they can access your Adobe Analytics credentials. 

If you require more robust password management solutions, there are packages that manage this, although I've not
investigated yet. It's on my to-do list and a pull request would be welcome if you wish to contribute. 


### Package structure



### Note to future developers
