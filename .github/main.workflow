workflow "Build and test on push" {
  on = "push"
  resolves = ["Compile"]
}

action "Compile" {
  uses = "docker://_/haskell"
  args = "stack build"
}
