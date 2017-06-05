#!/usr/bin/perl

$file = $ARGV[0];

if ($file =~ /\.cpp/) {
  while (<>) {
    print $_;
  }
}
else {
  @lines = <>;
  chomp(@lines);
  $brace = 0;

  for ($i = 0; $i < scalar @lines; ++$i) {
    $_ = $lines[$i];

    # adjust brace level
    $stm = $_;
    if ($$incmt) {
      if (m#^([^*]|\*+[^/*])*\*+/(.*)$#) {
        $stm = $1;
        $$incmt = 0;
      }
      else {
        $stm = "";
      }
    }
    $stm =~ s#//.*##;
    $stm =~ s#/\*([^*]|\*+[^/*])*\*+/##g;
    if ($stm =~ m#^(([^/]|/[^*])*)/\*#) {
      $$incmt = 1;
      $stm = $1;
    }

    ++$brace if $stm =~ /\{/;
    --$brace, $enum = 0 if $stm =~ /}/;


    # skip message handlers
    if ($lines[$i] =~ /^\s*long\s+on\w+\(\s*FXObject\s*\*\s*\w*\s*,\s*FXSelector\s*\w*\s*,\s*void\s*\*\s*\w*\s*\);/) {
      #splice @lines, $i, 0, ("  /** \@name Message Handlers */", "  //\@{ ");
      #$j = $i+2;
      #while ($lines[$j] =~ /^\s*long\s+on\w+\(\s*FXObject\s*\*\s*\w*\s*,\s*FXSelector\s*\w*\s*,\s*void\s*\*\s*\w*\s*\);/) {
      #  ++$j;
      #}
      #splice @lines, $j, 0, ("  //\@} ");
      #$i = $j;
      $lines[$i]="";
    }


    $skip = 0 if $brace <= 1;
    $skip = 1 if $stm =~ /^\s*public\s*:\s*$/;
    $skip = 2 if $stm =~ /^\s*protected\s*:\s*$/;
    $skip = 3 if $stm =~ /^\s*private\s*:\s*$/;
    $enum = 1 if $stm =~ /^\s*enum\s+\w*\s*\{[^}]*$/;
    if ($skip > 1) {
      splice @lines, $i, 1, ();
      --$i;
      next;
    }

    if (($lines[$i] =~ /^\*\// || @lines[$i] =~ m#///#)&& $lines[$i+1] =~ /^class/ && scalar @tags > 0 ) {
      if (@lines[$i] =~ m#///\s*(.*)$#) {
        @lines[$i] = "/** $1";
        splice @lines, $i+1, 0, ("*/");
        ++$i;
      }
      splice @lines, $i, 0, ("* ", "* See also: ", @tags);
      $i += scalar @tags + 2;
    }


    if ($enum == 1 && m#///?\s*.*$#) {
      $lines[$i] =~ s#///?#///<#;
    }

    if ($skip <= 1) {

      if ($brace == 1 && $lines[$i] =~ m|^\s*///?| && $lines[$i+1] =~ m|^\s*enum|) {
        $lines[$i] =~ m|^\s*///?\s*(.+)|;
        $cmt = $1;
        ($tag) = $file =~ /(\w+)\.\w+$/;
        $tag .= "_" . $group++;
        $tag =~ s#[ .:\\/]#_#g;
        push @tags, "* \\li \\ref $tag \"$cmt\".";
        splice @lines, $i, 0, ("/** \\addtogroup $tag $cmt", " * \@{", " */");
        $i += 3;
        $j = $i;
        while (!($lines[$j] =~ /\};/)) { ++$j; }
        splice @lines, $j+1, 0, ("/** @} */");
      }

      if ($brace == 3 && $lines[$i] =~ m#^\s*enum\s+\{#) {
        $j = $i;
        $delete = 1;
        while (!($lines[$j] =~ m#\}\s*;#)) {
          $delete = 0 if $lines[$j] =~ m#///?#;
          ++$j;
        }
        if ($delete) {
          splice @lines, $i, $j-$i+1;
          $enum = 0;
        }
      }
    }

  }

  foreach (@lines) {
    print $_ . "\n";
  }
}