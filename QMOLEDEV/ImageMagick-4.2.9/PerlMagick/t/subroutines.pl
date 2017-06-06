#
# Common subroutines to support tests
#
# Contributed by Bob Friesenhahn <bfriesen@simple.dallas.tx.us>
#

#
# Test reading a file
#
# Usage: testRead( read filename, expected md5 );
#
sub testRead {
  my( $infile, $md5 ) =  @_;

  my($image);
  
  $image=Image::Magick->new;
  $image->Set(size=>'512x512');
  $status=$image->ReadImage("$infile");
  if( "$status" ) {
    print "ReadImage $infile: $status";
    print "not ok $test\n";
  } else {
    $signature=$image->Get('signature');
    if ( $signature ne $md5 ) {
      print "Image: $infile, Computed: $signature, expected: $md5\n";
      print "not ok $test\n";
    } else {
      print "ok $test\n";
    }
  }
}

#
# Test reading a file which requires a file size to read (GRAY, RGB, CMYK)
# or supports multiple resolutions (JBIG, JPEG, PCD)
#
# Usage: testRead( read filename, size, expected md5 );
#
sub testReadSized {
  my( $infile, $size, $md5 ) =  @_;
  
  my($image);

  $image=Image::Magick->new;

  # Set size attribute
  $status=$image->SetAttribute(size=>"$size");
  warn "$status" if "$status";

  $status=$image->ReadImage("$infile");
  if( "$status" ) {
    print "ReadImage $infile: $status";
    print "not ok $test\n";
  } else {
    $signature=$image->Get('signature');
    if ( $signature ne $md5 ) {
      print "Image: $infile, Computed: $signature, expected: $md5\n";
      print "not ok $test\n";
    } else {
      print "ok $test\n";
    }
  }
}

#
# Test writing a file by first reading a source image, writing to a new image,
# reading the written image, and comparing with expected MD5.
#
# Usage: testReadWrite( read filename, write filename, write options, expected md5 );
#
# .e.g
#
# testReadWrite( 'input.jpg', 'output.jpg', q/quality=>80, interlace=>'None'/,
#                'dc0a144a0b9480cd1e93757a30f01ae3' );
#
# If the MD5 of the written image is not what is expected, the written
# image is preserved.  Otherwise, the written image is removed.
#
sub testReadWrite {
  my( $infile, $outfile, $writeoptions, $md5 ) = @_;
  
  my($image);

  $image=Image::Magick->new;
  $status=$image->ReadImage("$infile");
  $signature=$image->Get('signature');
  if( "$status" ) {
    print "ReadImage $infile: $status\n";
    print "not ok $test\n";
  } else {
    # Write image to file
    my $options = 'filename=>"$outfile", ' . "$writeoptions";
    #print "Using options: $options\n";
    eval "\$status=\$image->WriteImage( $options ) ;";
    if( $@ ) {
      print "$@\n";
      print "not ok $test\n";
      exit 1;
    }
    if( "$status" ) {
      print "WriteImage $outfile: $status\n";
      print "not ok $test\n";
    } else {
      my($image);

      # Read image just written
      $image=Image::Magick->new;
      $status=$image->ReadImage("$outfile");
      if( "$status" ) {
	print "ReadImage $outfile: $status\n";
	print "not ok $test\n";
      } else {
	# Check signature
	$signature=$image->Get('signature');
	if ( $signature ne $md5 ) {
	  print " Image: $outfile, Got: $signature, expected: $md5\n";
	  print "not ok $test\n";
	} else {
	  print "ok $test\n";
	  ($file = $outfile) =~ s/.*://g;
	  unlink "$file";
	}
      }
    }
  }
}

#
# Test writing a file by first reading a source image, writing to a
# new image, and reading the written image.  Depends on detecting
# reported errors by ImageMagick
#
# Usage: testReadWrite( read filename, write filename, write options);
#
# .e.g
#
# testReadWrite( 'input.jpg', 'output.jpg', q/quality=>80, 'interlace'=>'None'/ );
#
# If the read of the written image is not what is expected, the
# written image is preserved.  Otherwise, the written image is
# removed.
#
sub testReadWriteNoVerify {
  my( $infile, $outfile, $writeoptions) = @_;
  
  my($image, $images);
  
  $image=Image::Magick->new;
  $status=$image->ReadImage("$infile");
  if( "$status" ) {
    print "$status\n";
    print "ReadImage $infile: not ok $test\n";
  } else {
    # Write image to file
    my $options = 'filename=>"$outfile", ' . $writeoptions;
    #print "Using options: $options\n";
    eval "\$status=\$image->WriteImage( $options ) ;";
    if( $@ ) {
      print "$@";
      print "not ok $test\n";
      exit 1;
    }
    if( "$status" ) {
      print "WriteImage $outfile: $status\n";
      print "not ok $test\n";
    } else {
      my($image);

      # Read image just written
      $image=Image::Magick->new;
      $status=$image->ReadImage("$outfile");
      if( "$status" ) {
	print "ReadImage $outfile: $status\n";
	print "not ok $test\n";
      } else {
	print "ok $test\n";
	unlink $outfile;
      }
    }
  }
}

#
# Test writing a file by first reading a source image, writing to a new image,
# reading the written image, and comparing with expected MD5.
#
# Usage: testReadWriteSized( read filename, write filename, read filename size,
#                            write options, expected md5 );
#
# .e.g
#
# testReadWrite( 'input.jpg', 'output.jpg', '70x46', q/quality=>80,
#                'interlace'=>'None'/, 'dc0a144a0b9480cd1e93757a30f01ae3' );
#
# If the MD5 of the written image is not what is expected, the written
# image is preserved.  Otherwise, the written image is removed.
#
sub testReadWriteSized {
  my( $infile, $outfile, $size, $writeoptions, $md5 ) = @_;
  
  my($image);
  
  $image=Image::Magick->new;

  # Set size attribute
  $status=$image->SetAttribute(size=>"$size");
  warn "$status" if "$status";

  $status=$image->ReadImage("$infile");

  if( "$status" ) {
    print "ReadImage $infile: $status\n";
    print "not ok $test\n";
  } else {
    # Write image to file
    my $options = 'filename=>"$outfile", ' . "$writeoptions";
    #print "Using options: $options\n";
    eval "\$status=\$image->WriteImage( $options ) ;";
    if( $@ ) {
      print "$@\n";
      print "not ok $test\n";
      exit 1;
    }
    if( "$status" ) {
      print "WriteImage $outfile: $status\n";
      print "not ok $test\n";
    } else {
       my($image);

      $image=Image::Magick->new;

      # Set image size attribute
      $status=$image->SetAttribute(size=>"$size");
      warn "$status" if "$status";

      # Read image just written
      $status=$image->ReadImage("$outfile");
      if( "$status" ) {
	print "ReadImage $outfile: $status\n";
	print "not ok $test\n";
      } else {
	# Check signature
	$signature=$image->Get('signature');
	if ( $signature ne $md5 ) {
	  print " Image: $outfile, Got: $signature, expected: $md5\n";
	  print "not ok $test\n";
	} else {
	  print "ok $test\n";
	  ($file = $outfile) =~ s/.*://g;
	  unlink "$file";
	}
      }
    }
  }
}

#
# Test SetAttribute method
#
# Usage: testSetAttribute( name, attribute);
#
sub testSetAttribute {
  my( $srcimage, $name, $attribute ) = @_;

  my($image);
  
  # Create temporary image
  $image=Image::Magick->new;

  $status=$image->ReadImage("$srcimage");
  warn "Readimage: $status" if "$status";

  # Set image option
  print "Image Option  : $name=>$attribute\n";
  eval "\$status = \$image->Set('$name'=>'$attribute') ;";
  warn "SetImage: $status" if "$status";

  # Convert input values to expected output values
  $expected=$attribute;
  if ($attribute eq 'True' || $attribute eq 'true') {
    $expected = 1;
  } elsif ($attribute eq 'False' || $attribute eq 'false') {
    $expected = 0;
  }


  $value=$image->GetAttribute($name);

  if( defined( $value ) ) {
    if ("$expected" eq "$value") {
      print "ok $test\n";
    } else {
      print "Expected ($expected), Got ($value)\n";
      print "not ok $test\n";
    }
  } else {
    print "GetAttribute returned undefined value!\n";
    print "not ok $test\n";
  }
}

#
# Test GetAttribute method
#
# Usage: testGetAttribute( name, expected);
#
sub testGetAttribute {
  my( $srcimage, $name, $expected ) = @_;

  my($image);

  # Create temporary image
  $image=Image::Magick->new;

  $status=$image->ReadImage("$srcimage");
  warn "Readimage: $status" if "$status";

  $value=$image->GetAttribute($name);

  if( !defined( $expected ) && !defined( $value ) ) {
    # Undefined value is expected
    print "ok $test\n";
  } elsif ( !defined( $value ) ) {
    print "Expected ($expected), Got (undefined)\n";
    print "not ok $test\n";
  } else {
    if ("$expected" eq "$value") {
      print "ok $test\n";
    } else {
      print "Expected ($expected), Got ($value)\n";
      print "not ok $test\n";
    }
  }
}

#
# Test MontageImage method
#
# Usage: testMontage( input image attributes, montage options, expected MD5);
#
sub testMontage {
  my( $imageOptions, $montageOptions, $md5 ) = @_;

  my($image);

  # Create image for image list
  $images=Image::Magick->new;

  # Create temporary image
  $image=Image::Magick->new;

  my @colors = ( '#000000', '#008000', '#C0C0C0', '#00FF00',
		 '#808080', '#808000', '#FFFFFF', '#FFFF00',
		 '#800000', '#000080', '#FF0000', '#0000FF',
		 '#800080', '#008080', '#FF00FF', '#00FFFF' );
  
  my $color;
  foreach $color ( @colors ) {

    # Generate image
    $image->Set(size=>'50x50');
    $status=$image->ReadImage("xc:$color");
    warn "Readimage: $status" if "$status";

    # Add image to list
    push( @$images, @$image);
    
    undef @$image;
  }

  # Set image options
  #print "Image Options  : $imageOptions\n";
  eval "\$status = \$images->Set($imageOptions) ;";
  warn "SetImage: $status" if "$status";

  #print "Border color : ", $images->Get('bordercolor'), "\n";
  #print "Matte color  : ", $images->Get('mattecolor'), "\n";
  #print "Pen color    : ", $images->Get('pen'), "\n";

  # Do montage
  #print "Montage Options: $montageOptions\n";
  eval "\$montage=\$images->Montage( $montageOptions ) ;";
  if( $@ ) {
    print "$@";
    print "not ok $test\n";
    return 1;
  }
  
  if( ! ref($montage) ) {
    print "not ok $test\n";
  } else {
    #$montage->Display();
    # Check MD5 signature
    $signature=$montage->GetAttribute('signature');
    if ( defined( $signature ) ) {
      if ( $signature ne $md5 ) {
	print "Test $test Computed: $signature, expected: $md5\n";
	
        $status = $montage->Write("test_${test}_out.miff");
        warn "Write: $status" if "$status";
	
	print "not ok $test\n";
      } else {
	# Check montage directory
	my $directory = $montage->Get('directory');
	my $expected = join( "\n", @colors ) . "\n";
	if ( !defined($directory) ) {
	  print "ok $test\n";
	} elsif ( $directory  ne $expected) {
	  print("Invalid montage directory:\n\"$directory\"\n");
	  print("Expected:\n\"$expected\"\n");
	  print "not ok $test\n";
	} else {
	  # Check montage geometry
	  $montage_geom=$montage->Get('montage');
	  if( !defined($montage_geom) ) {
	    print("Montage geometry not defined!\n");
	    print "not ok $test\n";
	  } elsif ( $montage_geom !~ /^\d+x\d+\+\d+\+\d+$/ ) {
	    print("Montage geometry not in correct format: \"$montage_geom\"\n");
	    print "not ok $test\n";
	  } else {
	    print "ok $test\n";
	  }
	}
      }
    } else {
      warn "GetAttribute returned undefined value!";
      print "not ok $test\n";
    }
  }
}

#
# Test filter method
#
# Usage: testFilter( input image attributes, filter, options expected MD5);
#
sub testFilter {
  my( $srcimage, $filter, $options, $md5 ) = @_;

  my($image);

  # Create temporary image
  $image=Image::Magick->new;

  $status=$image->ReadImage("$srcimage");
  warn "Readimage: $status" if "$status";

  $image->$filter($options);

  $signature=$image->GetAttribute('signature');
  if ( defined( $signature ) ) {
    if ( $signature ne $md5 ) {
      print "Test $test Computed: $signature, expected: $md5\n";
      print "not ok $test\n";
    } else {
      print "ok $test\n";
    }
  } else {
    warn "GetAttribute returned undefined value!";
    print "not ok $test\n";
  }
}

1;
