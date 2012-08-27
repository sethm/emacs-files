My Emacs Configuration
======================

This is my emacs setup, and probably of interest to nobody but me.

Very recently, I've upgraded from emacs 23 to emacs 24, and with it
I'm trying an el-get package management setup. This is significantly
different to how I've worked in the past, but I'm willing to try to
change with the times.

I have set things up so that all installed el-get packages are ignored
by git, so each machine I clone my .emacs.d directory to will have to
bootstrap el-get and its packages on first install. I think I'm fine
with that.

The only caveat is that the el-get bootstrap process seems pretty
flakey so far. On both machines I've tried it on, I've had to restart
emacs several times to get all the packages installed. This should
therefore be considered a work in progress.

It's a brave new world!

Packages
--------

I have configured the following packages.

 - ruby-mode
 - ruby-compilation
 - css-mode
 - haml-mode
 - inf-ruby
 - rhtml-mode
 - rvm
 - textmate
 - yaml-mode
 - magit
 - magithub
 - dsvn