#!/usr/bin/perl -w
# Copyright © 2012 Stuart Hacking <stuhacking@gmail.com>
#
# Permission to use, copy, modify, distribute, and sell this software and its
# documentation for any purpose is hereby granted without fee, provided that the
# above copyright notice appear in all copies and that both that copyright
# notice and this permission notice appear in supporting documentation.  No
# representations are made about the suitability of this software for any
# purpose.  It is provided "as is" without express or implied warranty.
#
# Manages the installation/removal of system dotfiles.

require 5;
use strict;

my $overwrite_all = 0;
my $backup_all = 0;
my $skip_all = 0;

print STDOUT "Installing Dotfiles...\n";

my @symlinks = <"*/**.symlink">;

FILE_LOOP:
foreach my $symlink (@symlinks) {
    my $overwrite = 0;
    my $backup = 0;
    my $skip = 0;

    # Files ending in .symlink:
    my $file = $symlink;
    $file =~ s:.*/::g;
    $file =~ s:(.*)\.symlink:$1:;

    my $target = "$ENV{HOME}/.$file";

    if (-e $target) {
        unless ($backup_all || $skip_all || $overwrite_all) {
            print STDOUT "File already exists: $target.\n";
            print STDOUT '[s]kip, [S]kip all, [o]verwrite, [O]verwrite all, [b]ackup, [B]ackup all? ';

            my $line = <STDIN>;
            chomp($line);

            my $cmd = substr($line,0,1);

            if    ("o" eq $cmd) { $overwrite = 1; }
            elsif ("O" eq $cmd) { $overwrite_all = 1; }
            elsif ("b" eq $cmd) { $backup = 1; }
            elsif ("B" eq $cmd) { $backup_all = 1; }
            elsif ("s" eq $cmd) { $skip = 1; }
            elsif ("S" eq $cmd) { $skip_all = 1; }
            else {
                print STDERR "Unkown Option! -- $line\n";
                redo FILE_LOOP;
            }
        }

        `rm -fr $target` if $overwrite || $overwrite_all;
        `mv "$target" "$target.backup"` if $backup || $backup_all;

        next if $skip || $skip_all;
    }

    `ln -s "$ENV{PWD}/$symlink" "$target"`;
}


exit 0;
