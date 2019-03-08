workflow "Build and test on push" {
  on = "push"
  resolves = ["Compile"]
}

action "Compile" {
  uses = "docker://_/haskell"
  runs = "bash"
  args = "stack build"
}
