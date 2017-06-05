/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ groff / groff /
  tb
  s/ $/ groff /
  :b
  s/^/# Packages using this file:/
}
