#!/usr/local/bin/perl
#
# Test image filter.
#
# Contributed by Bob Friesenhahn <bfriesen@simple.dallas.tx.us>
#
BEGIN { $| = 1; $test=1, print "1..43\n"; }
END {print "not ok 1\n" unless $loaded;}
use Image::Magick;
$loaded=1;

require 't/subroutines.pl';

chdir 't' || die 'Cd failed';

testFilter('input.miff', 'Border',
  q/#ccc/,
  '36bc2833b05ad1e4e2cace027e045d9d');

++$test;
testFilter('input.miff', 'Blur',
  q/60/,
  '7669a6d89e4c47b3b98752f9ff9b8224');

++$test;
testFilter('input.miff', 'Border',
  q/10/,
  '60b20d26fbf98095cc994c21d013d42e');

++$test;
testFilter('input.miff', 'Chop',
  q/+10+10/,
  '92fd46db3668d557b12c30fc810b1fe6');

++$test;
testFilter('input.miff', 'ColorFloodfill',
  q/geometry=>+10+10,pen=>black/,
  'a51dc2d20c36c8e4df18afc567db636e');

++$test;
testFilter('input.miff', 'Contrast',
  q/1/,
  '60e07b732b6cd84177e0ba22a8aa4106');

++$test;
testFilter('input.miff', 'Crop',
  q/10x10/,
  '92fd46db3668d557b12c30fc810b1fe6');

++$test;
testFilter('input.miff', 'Colorize',
  q/red/,
  '6dc34477296f85d8584e655b3a429945');

++$test;
testFilter('input.miff', 'Contrast',
  q/1/,
  '60e07b732b6cd84177e0ba22a8aa4106');

++$test;
testFilter('input.miff', 'Despeckle',
  q/ /,
  '9166d0fff05dc5805eef1275863ec244');

++$test;
testFilter('input.miff', 'Edge',
  q/60/,
  '007f4247da95e7948816d0d17b1b6a2e');

++$test;
testFilter('input.miff', 'Emboss',
  q/ /,
  '28cddc797457c25ce339610cd1a8a374');

++$test;
testFilter('input.miff', 'Equalize',
  q/ /,
  'c484e3f1b74e35e911f514ad465451df');

++$test;
testFilter('input.miff', 'Flip',
  q/ /,
  'cac8dfaf79af279c6b1f37a495e8552f');

++$test;
testFilter('input.miff', 'Flop',
  q/ /,
  '64ce85ac3a977b2ddbeebb12690965c6');

++$test;
testFilter('input.miff', 'Frame',
  q/10x10/,
  '7558cc1e69972d2e0d9272b9e94dbaf6');

++$test;
testFilter('input.miff', 'Gamma',
  q/2.2/,
  'b5f821615ecb6f3856fef4a6939d9ad1');

++$test;
testFilter('input.miff', 'Implode',
  q/60/,
  'e0ccb026dd63abb1da662219e76980aa');

++$test;
testFilter('input.miff', 'Layer',
  q/Red/,
  'bf8f741ba7273610138b8e0ef5b5f79b');

++$test;
testFilter('input.miff', 'Magnify',
  q/ /,
  '7e926c0a076f567b61eeb08d553bc7e3');

++$test;
testFilter('input.miff', 'MatteFloodfill',
  q/geometry=>+10+10,matte=>Opaque/,
  'd7c89e6b946d94187dc34c58d4c5e2f1');

++$test;
testFilter('input.miff', 'Minify',
  q/ /,
  'b89f0cae97e3e8e1cd237fa8fac660b6');

++$test;
testFilter('input.miff', 'Modulate',
  q/60/,
  'e9b8baed761cc95bd60ba1b6272bad2b');

++$test;
testFilter('input.miff', 'Negate',
  q/0/,
  'e46a038426f4de5a3f051316aee46046');

++$test;
testFilter('input.miff', 'Normalize',
  q/ /,
  '352b86fce0e86dd83c326d257820be9c');

++$test;
testFilter('input.miff', 'OilPaint',
  q/3/,
  '8a103428f762f6e96c03fb52c78b90aa');

++$test;
testFilter('input.miff', 'Opaque',
  q/#fefefd/,
  '6444b7dae412d1d441b3ff8278e874d3');

++$test;
testFilter('input.miff', 'Quantize',
  q/128/,
  'c2635fa43f44da689b7ce9c33ca8b4aa');

++$test;
testFilter('input.miff', 'Raise',
  q/10x10/,
  '1e5e69f5c89a366cead97e4c71b97dc5');

++$test;
testFilter('input.miff', 'Roll',
  q/+10+10/,
  '6d1f073f2a9c2c7bb156e25c4436e691');

++$test;
testFilter('input.miff', 'Rotate',
  q/45/,
  'f2f0a9e4e1431863423d40088069554d');

++$test;
testFilter('input.miff', 'Sample',
  q/50%/,
  '3d541e271af26574314643698621c4bb');

++$test;
testFilter('input.miff', 'Scale',
  q/50%/,
  'ab8ba5119e2979ba7ba76d4a6d944b34');

++$test;
testFilter('input.miff', 'Segment',
  q/1/,
  '492cfb48dd301a0dad2cbf79afea916c');

++$test;
testFilter('input.miff', 'Shade',
  q/30x30/,
  'fbca87612e348428f4112a7912132c0e');

++$test;
testFilter('input.miff', 'Shear',
  q/45x45/,
  '494bb715137e5181d0783068ebe8c84c');

++$test;
testFilter('input.miff', 'Solarize',
  q/60/,
  '2594080986a34434961fe5841ae1bebc');

++$test;
testFilter('input.miff', 'Texture',
  q/filename=>granite:/,
  '7785d161ea66bd39394bb74ef704a0aa');

++$test;
testFilter('input.miff', 'Transform',
  q/geometry=>50x60/,
  '7785d161ea66bd39394bb74ef704a0aa');

++$test;
testFilter('input.miff', 'Threshold',
  q/128/,
  'ad5daf059a1f4dcd132f8aeed878611d');

++$test;
testFilter('input.miff', 'Trim',
  q/ /,
  '7785d161ea66bd39394bb74ef704a0aa');

++$test;
testFilter('input.miff', 'Wave',
  q/25x157/,
  '7b245cb58da477c1e4cce2a9626681cc');

++$test;
testFilter('input.miff', 'Zoom',
  q/geometry=>50%,filter=>Point/,
  'f9a5ccced93db546404214742d5329d2');

++$test;

1;
