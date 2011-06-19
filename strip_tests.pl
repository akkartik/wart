#!/usr/bin/env perl -w

$inside = 0;
$indent = '';
$match = 0;
while(<>) {
  chomp;
  if (!$inside) {
    if (!/{$/) {
      print $_, "\n";
    }
    else {
      $inside = 1;
      $indent = $_; $indent =~ s/[^ ].*//;
      $match = ($_ =~ /void test/);
      push @lines, $_;
    }
  }
  else {
    push @lines, $_;
    if (($indent eq "" || substr($_, 0, length($indent)) eq $indent) && substr($_, length($indent), 1) eq "}") {
      $inside = 0;
      print join("\n", @lines), "\n" unless $match;
      splice @lines;
    }
  }
}
print join("\n", @lines), "\n" unless $match;
