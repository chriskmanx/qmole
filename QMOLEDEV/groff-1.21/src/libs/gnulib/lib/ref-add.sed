/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ dummy / dummy /
  tb
  s/ $/ dummy /
  :b
  s/^/# Packages using this file:/
}
