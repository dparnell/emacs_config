emacs_config
============

This repository contains my Emacs configuration.
It assumes you are installing into a new machine with no existing Emacs configuration.

Requirements
============

Emacs 23 or 24

The default installation of Emacs under Mac OS X probably wont work so well for you, so install a more up to date version using [Homebrew](http://mxcl.github.io/homebrew/) or from [here](http://emacsformacosx.com/) for a GUI version.

Installation
============

    cd ~
    git clone https://github.com/dparnell/emacs_config.git .emacs.d
    cd .emacs.d
    git submodule init
    git submodule update
    ln -s .emacs.d/init.el .emacs

What you get
============

* Solarized colour theme
* Powerline for a nice looking mode line
* Ruby support
* Magit - git plugin
* nxhtml
* rails-reloaded
* scala mode
* rspec
* a clock in the mode line
* gforth support
* actionscript
* Emacs server started for emacsclient
* Cucumber mode
* Coffeescript mode
* SCSS mode
* Erlang mode
* Yaml mode
* Textile mode
* Mouse support in iTerm2
* PHP mode
* Multi-web-mode for editing files with different types of code in them (HTML/Javascript/CSS)
* Django style template mode
* Circe IRC client support

Other stuff
===========

The following extra commands are added to Emacs

* ```M-x iwb```  - indent the whole buffer
* ```C-c x``` - close all buffers
* ```M-x irc``` - connect to IRC

The config also attempts to make sure that TAB characters are not introduced into source files

Configuring IRC
===============

copy the ```private.el.example``` file to ```private.el``` and update the settings for your IRC server

