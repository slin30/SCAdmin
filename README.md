# SCAdmin

## Adobe Analytics Admin API functions

Version 0.0.0.9000 (in dev)

### How to install

There are several ways to get and install this package:

####From source

This is the most manual approach, but does not require actual access to the repository. All you 
need is the tarball (tar.gz; see below). Getting said tarball does require repository access, but
this method does allow anyone with the tarball to pass the file to others.

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

####Clone

This requires access to the repository, and you will need to either provide credentials, or pre-configure your
git environment to authenticate. 

```{r}
# In a git bash shell, type:
# dontrun{
# git clone https://git.cm-elsevier.com/zhang1/SCAdmin.git
# }
```

####Direct pull

This is the easiest method, as it can be performed within R itself. Instructions forthcoming once I've figured out
how to do this without passing credentials via clear text. 



### Key dependencies



### Package structure



### Note to future developers
