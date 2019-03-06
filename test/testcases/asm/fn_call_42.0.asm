; ModuleID = 'main'
source_filename = "<string>"

define double @fnNoArgs42_0() {
entry:
  ret double 4.200000e+01
}

define double @main() {
entry:
  %0 = call double @fnNoArgs42_0()
  ret double %0
}
