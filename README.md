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
##The basic pattern is: 'install.packages(path_to_file, repos = NULL, type="source")'
##For example:
# dontrun{
# install.packages("C:/Users/zhang2/Downloads/SCAdmin-master-37c6596be1b7afb2d65e0ecabcca9f87aa8812e6.tar.gz", 
#                  repos = NULL, 
#                  type="source") #Modify the path for your local system
# }
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
your gitlab user name and password, which can be done in any number of ways. The examples stores
my personal gitlab credentials in the .Renviron file; see 
[relevant instructions in Authentication](#a-bit-more-difficult-but-recommended-renviron)) for 
instructions on how to do this.

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

4. Restart your R session `<CTRL + SHIFT + F10>`

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



### Note to future developers
