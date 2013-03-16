#! /usr/bin/perl -w
# The Perl Speaking Alarm CLock
# Author: Stuart Hacking <stuhacking_AT_gmail.com>
# Version: 0.2

# Speaks a mixture of early morning greetings to gently encourage the
# user to wake up. Volume increases each time a certain number of
# iterations passes. phrases and names can be customized individually.

# Makes use of OS X commands for speech and volume control. Similar
# commands could probably be substituted on other Unix systems.
# Set the alarm by creating a cron entry, e.g:
#   30 6 * * * open -a Terminal.app -e ~/bin/alm.pl
require 5;
use strict;

my $volume = 1;
my $iteration = 0;
my $iter_delay = 4;

# Edit this to included any desired names/nicknames
# to be spoken.
my @names = ("Stewart","Lazybones");

# Edit this to add new phrases to the alarm.
# Instances of %NAME% will be replaced by a random
# name from @names.
my @phrases = (
    "%NAME%",
    "Wake Up!",
    "Wake Up, %NAME%!",
    "Hey, %NAME%!",
    "You don't want to be late!",
    "Up and atom.",
    "Rise and shine, %NAME%!",
    "Hey, %NAME%!",
    "Get up now!",
    "Rise and shine!",
    "Time to get up",
    "Good morning.",
    "I hope you are well rested.",
    "Busy day ahead.",
    "It's a beautiful day.");

# Speak a phrase using the OS X speech command.
sub osx_speech_command($) {
    my ($msg) = @_;
    `say "$msg"`;
}

# Set the OS X system volume.
sub osx_set_volume($) {
    my ($v) = @_;
    `osascript -e "set volume $v"`;
}

# Speak a random phrase.
sub speak_alarm() {
    my $phrase = $phrases[ rand @phrases ];
    my $name = $names[ rand @names ];

    $phrase =~ s/%NAME%/$name/;
    osx_speech_command($phrase);
}

# Increase the system volume.
sub inc_volume() {
    $volume = $volume + 1;

    osx_set_volume($volume);
}

# Reset the system volume to low.
sub reset_volume() {
    $volume = 0;
    inc_volume();
}

sub main() {
    reset_volume();
    do {
        
        speak_alarm();
        sleep $iter_delay;
        $iteration += 1;
        inc_volume() if ($iteration % 5 == 0);

    } while (1);
}

main();
exit 0;