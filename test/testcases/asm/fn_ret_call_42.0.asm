; ModuleID = 'main'
source_filename = "<string>"

define double @returnFirstArg(double %x) {
entry:
  ret double %x
}

define double @main() {
entry:
  %0 = call double @returnFirstArg(double 4.200000e+01)
  ret double %0
}
