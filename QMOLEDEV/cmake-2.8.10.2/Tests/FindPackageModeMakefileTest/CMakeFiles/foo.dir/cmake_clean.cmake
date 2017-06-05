FILE(REMOVE_RECURSE
  "CMakeFiles/foo.dir/foo.cpp.o"
  "libfoo.pdb"
  "libfoo.a"
)

# Per-language clean rules from dependency scanning.
FOREACH(lang CXX)
  INCLUDE(CMakeFiles/foo.dir/cmake_clean_${lang}.cmake OPTIONAL)
ENDFOREACH(lang)
