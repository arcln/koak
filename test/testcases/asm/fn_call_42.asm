; ModuleID = 'main'
source_filename = "<string>"

define i32 @fnNoArgs42() {
entry:
  ret i32 42
}

define i32 @main() {
entry:
  %0 = call i32 @fnNoArgs42()
  ret i32 %0
}
