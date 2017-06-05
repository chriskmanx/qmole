/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ texinfo / texinfo /
  tb
  s/ $/ texinfo /
  :b
  s/^/# Packages using this file:/
}
