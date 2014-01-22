#! /usr/bin/perl -w
# Copyright © 2012 Stuart Hacking <stuhacking@gmail.com>
#
# Permission to use, copy, modify, distribute, and sell this software and its
# documentation for any purpose is hereby granted without fee, provided that
# the above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation.  No representations are made about the suitability of this
# software for any purpose.  It is provided "as is" without express or 
# implied warranty.
#
# Manages the installation/removal of system dotfiles.
#
require 5;
use strict;
use Switch;

my $overwrite_all = 0;
my $backup_all = 0;
my $skip_all = 0;

sub install () {
    print STDOUT "Installing Dotfiles...\n";
    my @symlinks = <*/**.symlink>;

FILE_LOOP:
    foreach my $symlink (@symlinks) {
        my $overwrite = 0;
        my $backup = 0;
        my $skip = 0;

        my $file = $symlink;
        $file =~ s:.*/::g; # Strip directories
        $file =~ s:(.*)\.symlink:$1:;

        my $target = "$ENV{HOME}/.$file";

        if (-e $target) {
            unless ($backup_all || $skip_all || $overwrite_all) {
                print STDOUT "File already exists: $target.\n [s]kip, [S]kip all, [o]verwrite, [O]verwrite all, [b]ackup, [B]ackup all? ";
                my $line = <STDIN>;
                chomp($line);

                switch (substr($line,0,1)) {
                    case "o"      { $overwrite = 1; }
                    case "O"      { $overwrite_all = 1; }
                    case "b"      { $backup = 1; }
                    case "B"      { $backup_all = 1; }
                    case "s"      { $skip = 1;}
                    case "S"      { $skip_all = 1; }
                    else          { 
                        print STDERR "Unknown Option! -- $line\n"; 
                        redo FILE_LOOP;
                    }
                }            
            }

            `rm -fr $target` if $overwrite || $overwrite_all;
            `mv "$target" "$target.backup"` if $backup || $backup_all;

            next if $skip || $skip_all;
        }

        `ln -s "$ENV{PWD}/$symlink" "$target"`;
    }
}

sub uninstall () {
    print STDOUT "Uninstalling Dotfiles...\n";
    print STDERR "TODO: Uninstaller.\n";
}

sub usage () {
    print STDERR "Usage: perl dotfiles.pl [un]install\n";
    print STDERR "TODO: Extended Usage Message.\n";
    exit 1;
}

sub main () {

    if ($#ARGV < 0) { usage };

    while ($#ARGV >= 0) {
        $_ = shift @ARGV;
        if (m/^-?install$/)      { install; }
        elsif (m/^-?uninstall$/) { uninstall; }
        else                     { usage; }
    }
}

main();
exit 0;
