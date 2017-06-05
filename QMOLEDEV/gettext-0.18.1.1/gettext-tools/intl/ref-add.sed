/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ gettext-tools / gettext-tools /
  tb
  s/ $/ gettext-tools /
  :b
  s/^/# Packages using this file:/
}
