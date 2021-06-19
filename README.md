# dotlink

Dotfiles, (i.e. configuration files) are a mess. The [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) would be a good solution, if only applications would stick to it. As it is, there are a number of applications that are all too happy to dump their *.app_cache*, *.apprc* or *.appdata* into your ```$HOME``` directory, regardless of how you set ```$XDG_*_HOME``` environment variables. 

If you want to version-control your configuration files, this is a problem.

Using dotlink, you can not only treat your *configuration as code*, but their location in your system as well. This means you can write an *user.link* file like the following:
```
# You can specify which files to link where
link "dotfiles/xinitrc" "$HOME/.xinitrc"
link "dotfiles/Xresources" "$HOME/.Xresources"
link "dotfiles/zprofile" "$HOME/.zprofile"
link "dotfiles/profile" "$HOME/.profile"
link "dotfiles/zshrc" "$HOME/.zshrc"
link "dotfiles/inputrc" "$HOME/.inputrc"
link "dotfiles/bashrc" "$HOME/.bashrc"
link "dotfiles/bash_profile" "$HOME/.bash_profile"
link "dotfiles/Xresources.d" "$HOME/.Xresources.d"

# And you can hierarchycally include other .link files 
include "config.link"
include "scripts.link"
```
and dotlink will link all the files to their correct locations.

Dotlink also performs all the operations *atomically*, it will not make any changes to your system unless it has verified that you can make *all the changes* (for example, the link names are not existing non-symbolic-link files and you have relevant permissions to create such links).

Finally, you can keep all your configuration files in the same place commit them to a source control repository with ease!
